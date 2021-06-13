
#!/bin/bash

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
TS=$(date '+%Y%m%d_%H%M%S')

BACKUP_DIR=${SCRIPT_DIR}/backup/conf_repo_backup_${TS}
BACKUP_FILE=${SCRIPT_DIR}/backup/conf_repo_backup_${TS}.zip
mkdir -p ${BACKUP_DIR}

cp -r bin ${BACKUP_DIR}/
cp -r bash ${BACKUP_DIR}/

cd ${SCRIPT_DIR}
rsync -avr --exclude='path1/to/exclude' --exclude='path2/to/exclude' ./bin ${BACKUP_DIR}/
rsync -avr --include='bash/custom_bashrc.sh'  ./bash ${BACKUP_DIR}/
zip -r ${BACKUP_FILE} ${BACKUP_DIR}
 
