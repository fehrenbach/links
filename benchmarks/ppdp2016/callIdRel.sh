#!/bin/bash

set -e
set -o xtrace
# set -o pipefail

ns=(1000 10000 100000 1000000)
# ns=(1000)
ms=(8 16 32 64)
# ms=(8)

## echo create
## for n in ${ns[@]} ; do
##     for m in ${ms[@]} ; do
##         ./idrel -n $n -m $m > r_${n}_${m}.sql
##     done
## done

# echo create english
# for n in ${ns[@]} ; do
#     m=${ms[-1]}
#     ./idrel.lisp $n $m > i_s_c_o_${n}_${m}.sql
# done

# echo load for links
# for n in ${ns[@]} ; do
#     m=${ms[-1]}
#     psql -U postgres links -f i_s_c_o_${n}_${m}.sql
# done

# echo load for perm
# for n in ${ns[@]} ; do
#     m=${ms[-1]}
#     ~/tmp/bin/psql -p 23456 test -f i_s_c_o_${n}_${m}.sql
# done



# echo create links lineage queries
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         ./linksquery.hs -n $n -m $m > query_${n}_${m}_lineage.links
#     done
# done

# echo create links plain queries
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         ./linksquery_plain.hs -n $n -m $m > query_${n}_${m}_plain.links
#     done
# done


# echo run links queries
# echo "llinks" > debug_links_lineage.out
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         echo "NM: n $n m $m" >> debug_links_lineage.out
#         time ../../links --config=db.config query_${n}_${m}_lineage.links > /dev/null 2>> debug_links_lineage.out
#     done
# done

# echo run links queries plain
# echo "llinks plain" > debug_links_plain.out
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         echo "NM: n $n m $m" >> debug_links_plain.out
#         time ../../links --config=db.config query_${n}_${m}_plain.links > /dev/null 2>> debug_links_plain.out
#     done
# done

# echo run links queries plain NO SHREDDING
# echo "llinks plain" > debug_links_plain_no_shredding.out
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         echo "NM: n $n m $m" >> debug_links_plain_no_shredding.out
#         time ../../links --config=db.config query_${n}_${m}_plain.links > /dev/null 2>> debug_links_plain_no_shredding.out
#     done
# done

# echo create perm lineage queries
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         ./perm_query_lineage.hs -n $n -m $m > query_${n}_${m}_lineage.sql
#     done
# done

# echo create perm plain queries
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         ./perm_query_plain.hs -n $n -m $m > query_${n}_${m}_plain.sql
#     done
# done

# echo run perm lineage queries
# echo "perm" > debug_perm_lineage.out
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         echo "NM: n $n m $m" >> debug_perm_lineage.out
#         time ~/tmp/bin/psql -p 23456 test -f query_${n}_${m}_lineage.sql > /dev/null 2>> debug_perm_lineage.out
#     done
# done

# echo run perm plain queries
# echo "perm" > debug_perm_plain.out
# for n in ${ns[@]} ; do
#     for m in ${ms[@]} ; do
#         echo "NM: n $n m $m" >> debug_perm_plain.out
#         time ~/tmp/bin/psql -p 23456 test -f query_${n}_${m}_plain.sql > /dev/null 2>> debug_perm_plain.out
#     done
# done

echo run postgres plain queries
echo "postgres" > debug_postgres_plain.out
for n in ${ns[@]} ; do
    for m in ${ms[@]} ; do
        echo "NM: n $n m $m" >> debug_postgres_plain.out
        time psql -U postgres links -f query_${n}_${m}_plain.sql > /dev/null 2>> debug_postgres_plain.out
    done
done
