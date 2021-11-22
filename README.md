# nft-marketplace-server
## Images
### `GET /images`
* Response ```{"images":[{"path":"marketplace-images/c33554365c53292952de796095bd16cffc25502971b5c17b3299ea0a69c4256f_1.png","title":"My title2","sha256hash":"c33554365c53292952de796095bd16cffc25502971b5c17b3299ea0a69c4256f"}]}```
### `POST /images`
```curl -X POST /images -F "image=@Downloads/1.png" -F "title=My title2" ```
* Response
 ```{"sha256hash":"c33554365c53292952de796095bd16cffc25502971b5c17b3299ea0a69c4256f"}```

## Artists
### `GET /artists`
* Response ```{"artists":[{"name":"a","pubKeyHash":"b"},{"name":"ac","pubKeyHash":"bc"},{"name":"abc","pubKeyHash":"abc"}]}```
### `GET /artists/<pubKeyHash>`
Get artist name by the given `pubKeyHash`.
* Response ```{"name":"a"}```

## Purchases
### `GET /purchases/<imageHash>`
* Response ```{"purchases":[{"wasAuctioned":true,"authorPubKeyHash":"b","createdAt":"2021-11-17T12:41:28.165871Z","imageHash":"aaa","price":"d","ownerPubKeyHash":"c"}]}```

## Admin API
Auth by `Authorization: <token>` header.

### `POST /admin/unlist_image/<imageHash>`

### `POST /admin/create_artist`
* Request ```{"name":"abc", "pubKeyHash": "abc"}```
* Response ```{"name":"abc","pubKeyHash":"abc"}```

### `DELETE /admin/delete_artist`

### `POST /admin/create_purchase`
* Request 
```{"imageHash":"a", "authorPubKeyHash":"b", "ownerPubKeyHash":"c", "price":"d", "wasAuctioned":true }```
* Response ```{"wasAuctioned":true,"authorPubKeyHash":"b","createdAt":"2021-11-17T12:34:37.528854171Z","imageHash":"a","price":"d","ownerPubKeyHash":"c"}```
