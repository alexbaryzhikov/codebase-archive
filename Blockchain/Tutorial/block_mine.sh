NONCE=000000000
GOAL=000
LENGTH=3
SHA256=`sha256sum $1`

while [ "${SHA256:0:$LENGTH}" != "$GOAL" ]; do
    ((NONCE++))
    sed -i "1s/.*/$NONCE/" $1
    SHA256=`sha256sum $1`
    echo "$SHA256"
done
