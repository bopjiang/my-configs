
#!/bin/bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
TS=$(date '+%Y%m%d_%H%M%S')

BACKUP_TMP=${SCRIPT_DIR}/backup/tmp
BACKUP_DIRNAME=conf_repo_backup_${TS}
BACKUP_DIR=${BACKUP_TMP}/${BACKUP_DIRNAME}
BACKUP_FILE=${SCRIPT_DIR}/backup/conf_repo_backup_${TS}.tar.gz
mkdir -p ${BACKUP_DIR}

cp -r bin ${BACKUP_DIR}/
cp -r bash ${BACKUP_DIR}/

cd ${SCRIPT_DIR}
rsync -avr --exclude='path1/to/exclude' --exclude='path2/to/exclude' ./bin ${BACKUP_DIR}/
rsync -am --include='bash/custom_bashrc.sh' --exclude='*' ./bash ${BACKUP_DIR}/

cd ${BACKUP_TMP}
tar -czvf ${BACKUP_FILE} ${BACKUP_DIRNAME}
 
