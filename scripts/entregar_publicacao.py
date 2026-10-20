"""Entrega local de agregados já validados; não faz push nem acessa contas."""
from __future__ import annotations
import argparse, hashlib, json, os, pathlib, shutil

def digest(path):
    with path.open('rb') as handle:
        return hashlib.file_digest(handle, 'sha256').hexdigest()

def validar(stage):
    stage=pathlib.Path(stage).resolve()
    inventory=json.loads((stage/'publicacao/inventario_dados.json').read_text(encoding='utf-8-sig'))
    records=inventory['arquivos']
    if isinstance(records,dict): records=[records]
    files=[]
    for record in records:
        rel=pathlib.PurePosixPath(record['arquivo'])
        if rel.is_absolute() or '..' in rel.parts or rel.parts[0]!='data':
            raise ValueError('Caminho inválido no inventário de agregados.')
        source=(stage/pathlib.Path(*rel.parts)).resolve()
        if not source.is_relative_to(stage/'data') or not source.is_file():
            raise ValueError('Arquivo de dados fora da distribuição.')
        if digest(source)!=record['sha256']:
            raise ValueError('Hash não confere: '+str(rel))
        if source.suffix not in ('.rds','.json'):
            raise ValueError('Formato não permitido nos agregados.')
        files.append((rel,source))
    if not any(str(rel)=='data/estudo.rds' for rel,_ in files):
        raise ValueError('Estudo preservado ausente.')
    return stage,files

def entregar(stage,target):
    stage,files=validar(stage);target=pathlib.Path(target).resolve()
    if stage==target: raise ValueError('Distribuição e destino devem ser distintos.')
    for rel,source in files:
        destination=target/pathlib.Path(*rel.parts)
        destination.parent.mkdir(parents=True,exist_ok=True)
        if not destination.parent.resolve().is_relative_to(target):
            raise ValueError('Destino fora da aplicação.')
        temporary=destination.with_suffix(destination.suffix+'.entrega')
        shutil.copy2(source,temporary);os.replace(temporary,destination)
    destination=target/'publicacao/inventario_dados.json'
    destination.parent.mkdir(parents=True,exist_ok=True)
    shutil.copy2(stage/'publicacao/inventario_dados.json',destination)
    return len(files)

if __name__=='__main__':
    parser=argparse.ArgumentParser();parser.add_argument('pacote');parser.add_argument('destino')
    args=parser.parse_args()
    print(f'{entregar(args.pacote,args.destino)} arquivos agregados entregues. Nenhuma publicação externa executada.')
