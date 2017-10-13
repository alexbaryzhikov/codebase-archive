blocks_count=`ls | sort -n | grep -o '[0-9]*' | wc -l` # посчитать количество "блоков" с транзакциями
errors=0 # задать переменную, считающую несоответствия

for ((file=1;file<=$blocks_count;file++)) # пройдёмся по всем файлам подряд
do
    if [ $file != 1 ]; then # первый не трогаем
        hash_written=`tail -1 "$file.txt"` # читаем последнюю строку текущего файла
        hash_calculated=`sha256sum $(($file-1)).txt | awk '{print $1}'` # считаем хэш предыдущего
        if [ "$hash_written" != "$hash_calculated" ]; then # сравниваем
            ((errors++)) # если хэши не равны, инкрементим переменную
            echo "$file"
            echo "$hash_written"
            echo "$hash_calculated"
        fi
    fi
done

if [ $errors == 0 ]; then
    final_hash=`tail -1 final.txt` # читаем финальный хэш
    hash_calculated=`sha256sum $(($blocks_count)).txt | awk '{print $1}'` # считаем хэш последнего файла
    if [ "$final_hash" == "$hash_calculated" ]; then # сравниваем, и если всё в порядке, рапортуем в stdout
        echo "$blocks_count blocks"
        echo "chain is flawless"
        echo "final hash = $final_hash"
    fi
else
    echo "chain is corrupted" # или ругаемся, если в цепи есть ошибки
    echo "$errors"
fi
