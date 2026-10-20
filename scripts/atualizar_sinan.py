"""Coleta oficial, cache por ano, processamento e publicação atômica de agregados.
Execução: python -B scripts/atualizar_sinan.py [--forcar]
Só escreve dentro de painel/. Nenhum arquivo recebido é modificado.
"""
from __future__ import annotations
import argparse, datetime as dt, ftplib, hashlib, json, os, pathlib, shutil, subprocess, sys, time

ROOT = pathlib.Path(__file__).resolve().parents[1]
RUNTIME = pathlib.Path(os.environ.get('DASH_RUNTIME_DIR', ROOT / '.runtime')).resolve()
DATA = pathlib.Path(os.environ.get('DASH_DATA_DIR', ROOT / 'data')).resolve()
STATUS = DATA / 'atualizacao_status.json'

def agora():
    return dt.datetime.now().astimezone().isoformat(timespec='seconds')

def json_ler(p, padrao=None):
    return json.loads(p.read_text(encoding='utf-8-sig')) if p.exists() else (padrao or {})

def json_gravar(p, value):
    p.parent.mkdir(parents=True, exist_ok=True)
    tmp = p.with_suffix(p.suffix + '.tmp')
    tmp.write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding='utf-8')
    os.replace(tmp,p)

def sha(p):
    h=hashlib.sha256()
    with p.open('rb') as f:
        for b in iter(lambda:f.read(1024*1024),b''): h.update(b)
    return h.hexdigest()

def base_publicada_valida(pointer):
    value=pointer.get('arquivo')
    if not value: return False
    path=pathlib.Path(value)
    path=(path if path.is_absolute() else DATA/path).resolve()
    if not path.is_relative_to(DATA.resolve()) or not path.is_file(): return False
    return not pointer.get('sha256') or sha(path)==pointer['sha256']

def rscript():
    configured=os.environ.get('DASH_RSCRIPT')
    if configured:
        chosen=shutil.which(configured) or (configured if pathlib.Path(configured).is_file() else None)
        if not chosen: raise RuntimeError('DASH_RSCRIPT não aponta para um executável disponível.')
        return chosen
    found=shutil.which('Rscript')
    if found: return found
    candidates=sorted(pathlib.Path('C:/Program Files/R').glob('R-*/bin/Rscript.exe'),reverse=True) if os.name=='nt' else []
    p=next((p for p in candidates if p.exists()),None)
    if p is None: raise RuntimeError('Rscript não localizado; configure DASH_RSCRIPT.')
    return str(p)

def executar(forcar=False):
    RUNTIME.mkdir(parents=True,exist_ok=True); DATA.mkdir(parents=True,exist_ok=True)
    lock=RUNTIME/'atualizacao.lock'
    try: lock.mkdir()
    except FileExistsError:
        if time.time()-lock.stat().st_mtime < 8*3600:
            print('Já existe uma atualização em execução.'); return
        lock.rmdir(); lock.mkdir()
    previous=json_ler(STATUS)
    status={**previous,'ultima_tentativa':agora(),'estado':'em_andamento','mensagem':'Consultando o repositório oficial do DATASUS.'}
    json_gravar(STATUS,status)
    try:
        cfg=json_ler(ROOT/'config.json')
        cache=RUNTIME/'sinan_cache'; cache.mkdir(exist_ok=True)
        anterior=json_ler(cache/'catalogo.json', json_ler(DATA/'catalogo_sinan.json',{'arquivos':[]}))
        antigos={int(a['ano']):a for a in anterior.get('arquivos',[])}
        arquivos=[]; mudou=False
        with ftplib.FTP(cfg['ftp_host'],timeout=60) as ftp:
            ftp.login(); ftp.set_pasv(True)
            disponiveis={}
            fim=dt.date.today().year if cfg['ano_final']=='corrente' else int(cfg['ano_final'])
            for pasta in cfg['ftp_pastas']:
                ftp.cwd(pasta)
                for nome in ftp.nlst():
                    base=nome.rsplit('/',1)[-1]
                    if base.upper().startswith('IEXOBR') and base.lower().endswith('.dbc'):
                        yy=base[6:8]
                        if yy.isdigit() and cfg['inicio']<=2000+int(yy)<=fim:
                            # FINAIS tem preferência quando o mesmo ano aparece nas duas pastas.
                            disponiveis.setdefault(2000+int(yy),(pasta,base))
            if not all(y in disponiveis for y in range(2014,2026)):
                raise RuntimeError('O catálogo oficial não contém todos os anos de 2014 a 2025; mantida a última versão válida.')
            for ano,(pasta,nome) in sorted(disponiveis.items()):
                remoto=pasta+'/'+nome; ftp.voidcmd('TYPE I')
                tamanho=ftp.size(remoto)
                try: modificado=ftp.sendcmd('MDTM '+remoto)[4:]
                except ftplib.error_perm: modificado=None
                antigo=antigos.get(ano,{})
                local=cache/nome
                identico=(local.exists() and antigo.get('remoto')==remoto and
                          antigo.get('tamanho')==tamanho and modificado is not None and
                          antigo.get('modificado')==modificado and sha(local)==antigo.get('sha256'))
                if not identico or forcar:
                    print(f'Baixando {ano}: {tamanho} bytes',flush=True)
                    parcial=cache/(nome+'.part')
                    with parcial.open('wb') as f: ftp.retrbinary('RETR '+remoto,f.write,blocksize=1024*256)
                    if tamanho is not None and parcial.stat().st_size!=tamanho:
                        raise RuntimeError('Download incompleto: '+nome)
                    digest=sha(parcial)
                    # Arquivos modificados ganham uma cópia de evidência antes de trocar o cache.
                    if local.exists() and sha(local)!=digest:
                        hist=cache/'historico'; hist.mkdir(exist_ok=True)
                        anterior_hash=sha(local)
                        old=hist/(local.stem+'_'+anterior_hash[:16]+'.dbc')
                        if not old.exists(): shutil.copy2(local,old)
                    os.replace(parcial,local)
                    if digest!=antigo.get('sha256'): mudou=True
                else: digest=antigo['sha256']
                arquivos.append({'ano':ano,'arquivo':str(local),'remoto':remoto,'url':'ftp://'+cfg['ftp_host']+remoto,
                    'tamanho':tamanho,'modificado':modificado,'sha256':digest})
        pointer=DATA/'atualizacao_atual.json'
        assinatura=hashlib.sha256(json.dumps([(a['ano'],a['sha256']) for a in arquivos]).encode()).hexdigest()
        catalogo={'consultado_em':agora(),'arquivos':arquivos}
        json_gravar(cache/'catalogo.json',catalogo)
        if not mudou and pointer.exists() and json_ler(pointer).get('assinatura')==assinatura and base_publicada_valida(json_ler(pointer)) and not forcar:
            status.update(estado='sem_alteracoes',ultima_consulta=agora(),mensagem='Fonte consultada; sem novos arquivos ou revisões.')
            json_gravar(STATUS,status); print(status['mensagem']); return
        ident='sinan_'+dt.datetime.now().strftime('%Y%m%dT%H%M%S')
        dest=DATA/'atualizacoes'/ident; dest.mkdir(parents=True,exist_ok=False)
        job={**catalogo,'id':ident,'diretorio':str(dest)}
        jobfile=dest/'coleta.json'; json_gravar(jobfile,job)
        status.update(mensagem='Arquivos obtidos. Validando agregados, taxas, Bayes, Moran, LISA e tendências.')
        json_gravar(STATUS,status)
        env=os.environ.copy(); env.update(TMP=str(RUNTIME),TEMP=str(RUNTIME),TMPDIR=str(RUNTIME),LANG='en_US.UTF-8',LC_ALL='en_US.UTF-8',DASH_AMBIENTE='processamento',DASH_DATA_DIR=str(DATA),DASH_RUNTIME_DIR=str(RUNTIME))
        with (dest/'processamento.log').open('w',encoding='utf-8') as log:
            run=subprocess.run([rscript(),'--vanilla','scripts/processar_atualizacao.R',str(jobfile)],cwd=ROOT,env=env,
                               stdout=log,stderr=subprocess.STDOUT,timeout=7200)
        if run.returncode or not (dest/'dados.rds').is_file():
            raise RuntimeError('A validação não foi concluída. Consulte '+str(dest/'processamento.log'))
        json_gravar(pointer,{'id':ident,'arquivo':(dest/'dados.rds').relative_to(DATA).as_posix(),'atualizado_em':agora(),'assinatura':assinatura,'sha256':sha(dest/'dados.rds')})
        status.update(estado='atualizado',ultima_consulta=agora(),ultima_atualizacao=agora(),versao=ident,
                      mensagem='Nova versão validada e disponível no painel. A versão acadêmica foi preservada.')
        json_gravar(STATUS,status); print(status['mensagem'],flush=True)
    except Exception as e:
        status.update(estado='falha',ultima_consulta=agora(),mensagem=str(e))
        json_gravar(STATUS,status)
        print(str(e),file=sys.stderr,flush=True); raise
    finally:
        if lock.exists(): lock.rmdir()

if __name__=='__main__':
    p=argparse.ArgumentParser(); p.add_argument('--forcar',action='store_true'); args=p.parse_args()
    executar(args.forcar)
