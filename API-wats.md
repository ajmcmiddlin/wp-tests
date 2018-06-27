# API Wats

These are the confusing, complex, and annoying things about the API.

## Posts

WordPress offers one schema for posts, however it actually changes significantly depending on the
context.

### Timestamp ignored when creating posts

Two different dates may be provided -- GMT and local time -- however if both are provided, the GMT
date is ignored entirely as evidenced by providing two completely different times.

### Status changes based on current time

If a post is created with a status of 'future', but a date in the past, it is immediately changed to
published.

More understandably, if a post has status 'future' and its publication date comes and goes, it will
be bumped to published.

### It's an object... sometimes

Content, title, excerpt, and guid are all objects -- except when they're not. When creating a post,
the fields we can provide (all except guid) are all just text values. The created post we get back,
and the posts returned when listing posts, both have objects for each of these fields. Even then,
the fields in these objects differs when being returned from a create, and when returned from a
listing.

### Slugs man...

When creating posts:

* If an unused slug is provided -- that slug is used.
* If an empty slug or no slug field are provided -- a slug is generated.
* If a slug is provided but would break uniqueness -- a number is appended to differentiate it.
* If a slug is provided that contains formatting or capitals -- they are all stripped such that it
  contains only lower case alphanumeric characters and hyphens.
* If a slug is too long, it is truncated to 200 characters apparently.

