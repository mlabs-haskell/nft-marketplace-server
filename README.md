# nft-marketplace-server
## Images
### `GET /images` 
Response with headers:
```
< HTTP/1.1 206 Partial Content
< Transfer-Encoding: chunked
< Date: Tue, 23 Nov 2021 15:55:40 GMT
< Server: Warp/3.3.17
< Content-Type: application/json;charset=utf-8
< Total-Count: 571
< Accept-Ranges: createdAt
< Content-Range: createdAt 2021-11-23T10%3A45%3A10.310676Z..2021-11-23T10%3A45%3A10.310676Z
< Next-Range: createdAt 2021-11-23T10%3A45%3A10.310676Z;limit 1;offset 1;order desc
<

[{"path":"marketplace-images/d20906d82638fe30cef8e559e84ddbee4f4b965dd6a066e4c9d03d3295dadfff_1.png","createdAt":"2021-11-23T10:45:10.310676Z","id":22651,"title":"benchmark title","sha256hash":"d20906d82638fe30cef8e559e84ddbee4f4b965dd6a066e4c9d03d3295dadfff"}]
```
#### Pagination
* Header `Total-Count` -- total amount of entries
* Default number of entries in a page is 100
* To get next page send a request with the header `Range: <nextRange>` where value of `<nextRange>` is from the `Next-Range` header.
* Example with `curl`:
```curl -X GET /images -vH 'Range: createdAt 2021-11-23T10%3A45%3A10.31487Z;limit 1;offset 1;order desc```


### `POST /images`
```curl -X POST /images -F "image=@Downloads/1.png" -F "title=My title2" ```
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
