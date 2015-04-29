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
Get identifier details  | /identifier_name   | `GET`     | n/a           | Example 1 
Create identifier       | /identifier_name   | `POST`    | Example 2     | true/false
Delete identifier       | /identifier_name   | `DELETE`  | n/a           | true/false

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
get link        | /link/vname1%3Avname2  | `GET`     | n/a       | Example 4
delete link     | /graph_name/link/vname1%3Avname2  | `DELETE`  | n/a       | true/false

> %3A is the urlencoded of ":" .
> A link name is created by appending the linked identifiers with a colon in the middle, Ex: name1:name2.
> Missing vertices will be created, when adding a link. the allowed "_type" values currently are "resource" and "id" as seen below in Example 3.
> Links are not directed so link names are symmetric.  That is name1:name2 is the same as name2:name1.

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
    "link": "/link/vname1%3Avname2",
    "identifiers": [
        "vname1",
        "vname2"
    ],
    "metadata": {
        "ip": "10.151.1.68"
    }
}
```

-------

###Metadata 

Description | URI | Method | Request Body | Response Body
--- | --- | --- | --- | ---
get identifier metadata             | /identifier/identifier_name/metadata       | `GET`     | n/a       | Example 5
get link metadata                   | /link/vname1%3Avname2/metadata             | `GET`     | n/a       | Example 5
get identifier metadata property    | /identifier/identifier_name/ip             | `GET`     | n/a       | "1"
get link metadata property          | /link/vname%3Avname2/creation_datetime     | `GET`     | n/a       | "2014-07-16T19:20:30+01:00"
add identifier metadata             | /identifier/identifier_name/metadata/ip    | `POST`    | "2.2.2.2" | true/false
add link metadata                   | /link/vname%3Avname2/metadata/key          | `POST`    | "value"   | true/false
remove identifier metadata          | /identifier/identifier_name/metadata/ip    | `DELETE`  | n/a       | true/false
remove link metadata                | /link/vname%3Avname2/metadata/key          | `DELETE`  | n/a       | true/false

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
search on link        | /link/link1/search           |   `POST`  |   Example 6 | Example 7

###Example 6 - search request JSON body
```
{
  "Id": "id",
  "Type": "IPV4",
  "max_depth": "1",
  "max_size": "10000",
  "match_links": "Type: IP-MAC",
  "results-filter": "meta:capabilities",
  "terminal-type": "Type:device"
}
```
> id = identifier/link id
> Id and Max Depth is implemented.
> Type(Identifier Type), match_links, results-filter and terminal-type is in progress.

###Example 7 - search response JSON
```
{
    "identifiers": [
        "vertex1",
        "vertex2"
    ],
    "links": [
        "vertex1%3Avertex2"
    ]
}
```
> NOTE: probably want to make identifers and links elements maps so we can include the metaadata.
