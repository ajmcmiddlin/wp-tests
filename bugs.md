## content-empty

### Endpoint

wp-json/wp/v2/posts

### Body

```json
 {
    "status": "publish",
    "slug": "",
    "content": {
        "protected": false,
        "rendered": "a"
    },
    "author": 1,
    "date_gmt": "1900-01-01T12:00:00Z",
    "excerpt": {
        "protected": false,
        "rendered": "a"
    },
    "title": {
        "rendered": "a"
    }
}
```

### Response

400 Bad Request

```json
{
  "code": "empty_content",
  "message": "Content, title, and excerpt are empty.",
  "data": {
    "status": 400
  }
}
```
