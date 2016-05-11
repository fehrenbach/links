#!/bin/bash

set -e
set -o xtrace

LINKSROOT="../../"

function drop_tables {
    psql -v ON_ERROR_STOP=1 -q -U postgres -d links -f "droptables.sql"
}

function recreate_tables {
    psql -v ON_ERROR_STOP=1 -q -U postgres -d links -f "organisation.sql"
}

function populate {
    "$LINKSROOT/links" --config=nodebug.config setup.links
}

function noprov {
    "$LINKSROOT/links" --config=db.config noprov.links
}

function allprov {
    "$LINKSROOT/links" --config=db.config allprov.links
}

function someprov {
    "$LINKSROOT/links" --config=db.config someprov.links
}

function lineage {
    "$LINKSROOT/links" --config=db.config lineage.links
}

function nolineage {
    "$LINKSROOT/links" --config=db.config nolineage.links
}

# Dump
# pg_dump links -t departments -t employees -t tasks -t contacts > filename.sql

function loadDump {
    drop_tables
    psql -U postgres links < "$1.sql"
    psql -v ON_ERROR_STOP=1 -q -U postgres -d links -f "indices.sql"
}

# Restore from dump
# psql -U postgres links < filename.sql

# drop_tables
# recreate_tables
# time populate
# time noprov

function prepareCSV {
    # echo '"prov";"N";"query";"medianms";"timems"' > data.csv
    echo '"prov";"N";"query";"medianms"' > data.csv
}

function sanitizeCSV {
    sed -i '/() : ()/d' data.csv
    sed -i '/^Q/d' data.csv
    sed -i '/^AQ/d' data.csv
}

# sizes=(4096 2048 1024 512 256 128 64 32 16 8 4)
# sizes=(1024 512 256 128 64 32 16 8 4)
# sizes=(1024 512 256 128 64 32 16 8 4)
sizes=(128 64 32 16)

prepareCSV
for n in "${sizes[@]}"
do
    loadDump $n
    # allprov >> data.csv
    # someprov >> data.csv
    # noprov >> data.csv
    lineage >> data.csv
    nolineage >> data.csv
done
sanitizeCSV
