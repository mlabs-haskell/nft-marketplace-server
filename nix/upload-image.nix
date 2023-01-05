{ pkgs, ... }:

pkgs.writeShellApplication {
  name = "upload-image";
  runtimeInputs = with pkgs; [ python39Packages.httpie ipfs ];
  text = ''
    if [ $# != 4 ]; then
        echo "Arguments: <IMAGE_FILE> <TITLE> <DESCRIPTION>"
        exit 1
    fi

    IMAGE=$1
    TITLE=$2
    DESC=$3
    SEABUG_ADMIN_TOKEN=$4
    BACKEND_PORT=8008

    ############################################################
    # Prepare
    ############################################################

    # Setup server admin token, password: seabug
    psql -U seabug -h localhost -q -c "INSERT INTO admin_token(token) VALUES ('$SEABUG_ADMIN_TOKEN') ON CONFLICT DO NOTHING"

    ############################################################
    # Functions
    ############################################################

    get_ipfs_hash() {
        local IMAGE_HASH=$1
        http GET localhost:$BACKEND_PORT/images |
            jq -r "to_entries[] | select (.value.sha256hash == \"$IMAGE_HASH\") | .value.ipfsHash"
    }

    ############################################################
    # Upload image
    ############################################################

    echo '>' Image upload...
    BUF=$(
        http --form POST localhost:$BACKEND_PORT/admin/upload_image \
            "Authorization:$SEABUG_ADMIN_TOKEN" \
            "files@$IMAGE" \
            "title=$TITLE" \
            "description=$DESC" \
            --pretty none
    )
    IMAGE_HASH=$(echo -n "$BUF" | rg '^\{' | jq -r '.sha256hash')
    if [ -z "$IMAGE_HASH" ] || [ "$IMAGE_HASH" = "null" ]; then
        echo Upload image error: $BUF
        exit 1
    fi
    echo '>' Image upload...ok
    echo '>' IMAGE_HASH: $IMAGE_HASH
    IPFS_HASH=$(get_ipfs_hash $IMAGE_HASH)
    echo '>' IPFS_HASH: $IPFS_HASH
    export IPFS_CID=$(ipfs cid format -b base36 $IPFS_HASH)
    echo '>' IPFS Base36 CID: $IPFS_CID
  '';
}
