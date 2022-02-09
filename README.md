# nft-marketplace-server
## Images
### `GET /images` 
Response with headers:
```
< HTTP/1.1 206 Partial Content
< Transfer-Encoding: chunked
< Date: Wed, 09 Feb 2022 21:57:23 GMT
< Server: Warp/3.3.17
< Content-Type: application/json;charset=utf-8
< Total-Count: 0
< Accept-Ranges: createdAt
< Content-Range: createdAt 2022-02-09T21%3A55%3A34.31034Z..2022-02-09T21%3A55%3A34.31034Z
< Next-Range: createdAt 2022-02-09T21%3A55%3A34.31034Z;limit 100;offset 1;order desc
<
* Connection #0 to host localhost left intact
[{"path":"marketplace-images/62ae402c7011b974019c2ca58af369100b53c32593a69de8cf3db8d7e09f5251_1.png","createdAt":"2022-02-09T21:55:34.31034Z","ipfsHash":"bafkreidcvzacy4arxf2adhbmuwfpg2iqbnj4gjmtu2o6rtz5xdl6bh2ske","id":54005,"title":"My title2","description":"image desc","sha256hash":"62ae402c7011b974019c2ca58af369100b53c32593a69de8cf3db8d7e09f5251"}]
```
#### Pagination
* Header `Total-Count` -- total amount of entries
* Default number of entries in a page is 100
* To get next page send a request with the header `Range: <nextRange>` where value of `<nextRange>` is from the `Next-Range` header.
* Example with `curl`:
```curl -X GET /images -vH 'Range: createdAt 2021-11-23T10%3A45%3A10.31487Z;limit 1;offset 1;order desc```


### `POST /images`
```curl -X POST /images -F "image=@Downloads/1.png" -F "title=My title2" -F "description=image description"```
* Response
 ```{"sha256hash":"c33554365c53292952de796095bd16cffc25502971b5c17b3299ea0a69c4256f"}```

## Artists
### `GET /artists`
Response with headers:
```
< HTTP/1.1 206 Partial Content
< Transfer-Encoding: chunked
< Date: Thu, 25 Nov 2021 13:17:15 GMT
< Server: Warp/3.3.17
< Content-Type: application/json;charset=utf-8
< Total-Count: 1
< Accept-Ranges: createdAt
< Content-Range: createdAt 2021-11-25T13%3A16%3A49.023987Z..2021-11-25T13%3A16%3A49.023987Z
< Next-Range: createdAt 2021-11-25T13%3A16%3A49.023987Z;limit 100;offset 1;order desc
<

[{"createdAt":"2021-11-25T13:16:49.023987Z","name":"abc","pubKeyHash":"abc","id":4}]
```

#### Pagination
See image pagination.

### `GET /artists/<pubKeyHash>`
Get artist name by the given `pubKeyHash`.
* Response ```{"name":"a"}```

## Purchases
### `GET /purchases/<imageHash>`
* Response ```{"purchases":[{"wasAuctioned":true,"authorPubKeyHash":"b","createdAt":"2021-11-17T12:41:28.165871Z","imageHash":"aaa","id":9,"price":"d","ownerPubKeyHash":"c"}]}```

## Admin API
Auth by `Authorization: <token>` header.

### `POST /admin/unlist_image/<imageHash>`

### `POST /admin/create_artist`
* Request ```{"name":"abc", "pubKeyHash": "abc"}```
* Response ```{"name":"abc","pubKeyHash":"abc"}```

### `DELETE /admin/delete_artist/<pubKeyHash>`

### `POST /admin/create_purchase`
* Request 
```{"imageHash":"a", "authorPubKeyHash":"b", "ownerPubKeyHash":"c", "price":"d", "wasAuctioned":true }```
* Response ```{"wasAuctioned":true,"authorPubKeyHash":"b","createdAt":"2021-11-17T12:34:37.528854171Z","imageHash":"a","price":"d","ownerPubKeyHash":"c"}```
