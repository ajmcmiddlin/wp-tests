## Concurrent deletes

### Reproduce

**commit**: aa005ed4d90b56339ed896f5cc2820ec4ef7cc1f
**command**: `./run ./dist/build/state-tests/state-tests -p parallel '--hedgehog-replay "Size 43 Seed 6096737530672411682 (-8873261708108371543)"'`

### Result

```
112 ┃     f cs s = forAll $ Gen.parallel (Range.linear 1 100) (Range.linear 1 10) s cs
              ┃     │ ━━━ Prefix ━━━
              ┃     │ Var 25 = CreatePost
              ┃     │            (fromList
              ┃     │               [ PostDateGmt :=> Identity 1900 (-01) (-01) 12 : 00 : 00
              ┃     │               , PostSlug :=> Identity (Slug "a")
              ┃     │               , PostStatus :=> Identity Publish
              ┃     │               , PostTitle :=> Identity (R (L (RCreate "a")))
              ┃     │               , PostContent :=> Identity (RP (L (PRCreate "a")))
              ┃     │               , PostAuthor :=> Identity (Author 1)
              ┃     │               , PostExcerpt :=> Identity (RP (L (PRCreate "a")))
              ┃     │               ])
              ┃     │ 
              ┃     │ ━━━ Branch 1 ━━━
              ┃     │ Var 26 = DeletePost (Var 25) Nothing
              ┃     │ Var 27 = DeletePost (Var 25) Nothing
              ┃     │ 
              ┃     │ ━━━ Branch 2 ━━━
              ┃     │ Var 28 = DeletePost (Var 25) (Just True)

...

no valid interleaving
```

### Notes

In an attempt to inspect the failure, I manually created a post, and then ran a simple exe (code
below, git commit `3b89bf6f1b16bb62931c931ef3806dac06dbba73**) to print the result of running the
deletes from each branch concurrently.

As can be seen in the output, it appears that the force delete succeeds (`Right` value returned)
while the other delete fails with a `DecodeFailure`. The response body (below) looks like some sort
of default object where many fields are `null`.

**code***

```haskell
main ::
  IO ()
main = do
  Env{..} <- mkEnv
  (postId:_) <- getArgs
  let
    auth = BasicAuthData wpUser wpPassword
    d force = runClientM (deletePost auth (read postId) force) servantClient
    d1 = d Nothing
    d2 = d $ Just True
  (r1, r2) <- concurrently d1 d2
  print r1
  print r2

```

**command**

`./run cabal run manual-test -- 10`

**output**

```haskell
Left (DecodeFailure {decodeError = "Error in $: could not parse date: not enough input", responseContentType = application/json;charset=UTF-8, responseBody = "{\"id\":null,\"date\":\"\",\"date_gmt\":\"\",\"guid\":{\"rendered\":null,\"raw\":null},\"modified\":\"\",\"modified_gmt\":\"\",\"password\":null,\"slug\":null,\"status\":null,\"type\":null,\"link\":false,\"title\":{\"raw\":null,\"rendered\":\"\"},\"content\":{\"raw\":null,\"rendered\":\"\",\"protected\":false},\"excerpt\":{\"raw\":null,\"rendered\":\"\",\"protected\":false},\"author\":0,\"featured_media\":0,\"comment_status\":null,\"ping_status\":null,\"sticky\":false,\"template\":\"\",\"format\":\"standard\",\"meta\":[],\"categories\":[],\"tags\":[],\"_links\":{\"self\":[{\"href\":\"http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/posts\\/\"}],\"collection\":[{\"href\":\"http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/posts\"}],\"about\":[{\"href\":\"http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/types\\/post\"}],\"wp:attachment\":[{\"href\":\"http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/media\"}],\"curies\":[{\"name\":\"wp\",\"href\":\"https:\\/\\/api.w.org\\/{rel}\",\"templated\":true}]}}"})
Right (fromList [])
```

**formatted JSON response**

```json
{
    "id": null,
    "date": "",
    "date_gmt": "",
    "guid": {
        "rendered": null,
        "raw": null
    },
    "modified": "",
    "modified_gmt": "",
    "password": null,
    "slug": null,
    "status": null,
    "type": null,
    "link": false,
    "title": {
        "raw": null,
        "rendered": ""
    },
    "content": {
        "raw": null,
        "rendered": "",
        "protected": false
    },
    "excerpt": {
        "raw": null,
        "rendered": "",
        "protected": false
    },
    "author": 0,
    "featured_media": 0,
    "comment_status": null,
    "ping_status": null,
    "sticky": false,
    "template": "",
    "format": "standard",
    "meta": [],
    "categories": [],
    "tags": [],
    "_links": {
        "self": [
            {
                "href": "http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/posts\\/"
            }
        ],
        "collection": [
            {
                "href": "http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/posts"
            }
        ],
        "about": [
            {
                "href": "http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/types\\/post"
            }
        ],
        "wp:attachment": [
            {
                "href": "http:\\/\\/192.168.56.106\\/wp-json\\/wp\\/v2\\/media"
            }
        ],
        "curies": [
            {
                "name": "wp",
                "href": "https:\\/\\/api.w.org\\/{rel}",
                "templated": true
            }
        ]
    }
}
```

### report

[posted to WordPress's trac 20180711T1230Z](https://core.trac.wordpress.org/ticket/44568)

```
=== Steps to reproduce

Using the REST API:

* Create a post
* Run two deletes for created post concurrently:
   1. Force delete (query params: `?force=true`)
   2. Delete without forcing (do not specify `force` query parameter)

=== Expected

One delete to return `200 OK` and a valid return value. The other to return a `404 Not Found` or `410 Gone` depending on which one was successful.

=== Result

1. Force delete returns `200 OK` and an empty post object (normally returns a JSON object including keys `deleted` and `previous`).
2. Delete without force returns a post object full of `null` (see below)

INSERT OUTPUT FROM ABOVE

=== Notes

* WordPress is latest release version at time of writing (4.9.7)
* Server OS is Linux (running in a VM with fresh install)
* Default theme is installed
* Basic-Auth plugin is the only plugin installed -- required for testing``
```
