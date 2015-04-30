# dobby_rest
RESTish interface to dobby, the graph store

###HTTP response codes

Response code | Description
--- | ---
200   | Success
201   | Successfully created resource
204   | Successfully deleted resource
404   | Not Found
422   | POST request malformed/incomplete
500   | Server Error

###Identifiers

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
Get identifier details  | /identifier/identifier_name   | `GET`     | n/a           | Example 1 
Create identifier       | /identifier/identifier_name   | `POST`    | Example 2     | true/false
Delete identifier       | /identifier/identifier_name   | `DELETE`  | n/a           | true/false

> Note: Deleting a identifier will remove associated links.

####Example 1 - identifier JSON response object:

```
{
    "identifier":"name2",
    "metadata":{
       "key":"1",
       "key2":"2"
    }
}
```

####Example 2 - create identifier JSON request body

```
{"metadata":{"key":"1","key2":"2"}}
```

###Links

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
create link     | /link                  | `POST`    | Example 3 | true/false
get link        | /link/vname1/vname2  | `GET`     | n/a       | Example 4
delete link     | /link/vname1/vname2  | `DELETE`  | n/a       | true/false

> A link name is created by appending the linked identifiers with a slash (/) in the middle, Ex: name1/name2.
> Missing vertices will be created, when adding a link.
> Links are not directed so link names are symmetric.  That is name1/name2 is the same as name2/name1.

####Example 3 - create link request body

```
{
  "link":[
    {
      "identifier":"vname1",
      "metadata":{
        "type":"resource",
        "ip":"9.9.9.9"
      }
    },
    {
      "identifier":"vname2",
      "metadata":{
        "type":"id",
        "ip":"10.151.1.71"
      }
    }
  ],
  "metadata":{
    "ip": "10.151.1.68"
  }
}
```

####Example 4 - link JSON response object

```
{
  "link":[
    {
      "identifier":"vname1",
      "metadata":{
        "type":"resource",
        "ip":"9.9.9.9"
      }
    },
    {
      "identifier":"vname2",
      "metadata":{
        "type":"id",
        "ip":"10.151.1.71"
      }
    }
  ],
  "metadata":{
    "ip": "10.151.1.68"
  }
}
```

-------

###Metadata 

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
get identifier metadata             | /identifier/identifier_name/metadata       | `GET`     | n/a       | Example 5
get link metadata                   | /link/vname1/vname2/metadata             | `GET`     | n/a       | Example 5
get identifier metadata property    | /identifier/identifier_name/ip             | `GET`     | n/a       | "1"
get link metadata property          | /link/vname/vname2/creation_datetime     | `GET`     | n/a       | "2014-07-16T19:20:30+01:00"
add identifier metadata             | /identifier/identifier_name/metadata/ip    | `POST`    | "2.2.2.2" | true/false
add link metadata                   | /link/vname1/vname2/metadata/key          | `POST`    | "value"   | true/false
remove identifier metadata          | /identifier/identifier_name/metadata/ip    | `DELETE`  | n/a       | true/false
remove link metadata                | /link/vname1/vname2/metadata/key          | `DELETE`  | n/a       | true/false

####Example 5 - metadata JSON
```
{
    "metadata":{
       "key":"1",
       "key2":"2"
    }
}
```

###Search

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
search on identifier  | /identifier/vname1/search    |   `POST`  |   Example 6 | Example 7

###Example 6 - search request JSON body
```
{
  "max_depth":"1",
  "traversal":"depth",
  "max_size":"100",
  "match_metadata":{
      "type":"IPV4"
  },
  "match_links":{
    "type":"IP-MAC"
  }
  "results_filter":["capabilities"],
  "match_terminal":{
    "type":"device"
  }
}
```
> max_depth - maximum search depth from starting identifier (e.g., vname1)
> traversal - graph traversal algorithm ("depth" for depth first or "breadth" for breadth first)
> max_size - maximum number of identifiers in the result set
> match_metadata - only follow links from identifiers with metadata matching these key/value pairs.  The identifier's metadata must match all of the key.values pairs.
> match_links - only follow links with metadata matching these key/value pairs.  The link's metadata must match all of the key/value pairs.
> results_filter - list of metadata keys to include in the metadata in the results.  If not given or "all" (instead of a list), then all of the metadata is included.
> match_terminal - do not follow links from an identifer that has metadata matching these key/value pairs.
> result is a list of identifiers and links.
> FIRST RELEASE implements only max_depth, traversal, and max_size.

###Example 7 - search response JSON
```
[
    {
      "identifier":"vname1",
      "metadata":{
        "type":"resource",
        "ip":"9.9.9.9"
      }
    },
    {
      "identifier":"vname2",
      "metadata":{
        "type":"resource",
        "ip":"8.8.8.8"
      }
    },
    {
      "link":"vname1/vname2"
      "metadata":{
        "type":"connection"
      }
    }
]
```
