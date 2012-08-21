Backplane 1.2 README
====================


Backplane API
-------------

The Backplane Server exposes the following Backplane API endpoints,
as defined by [Backplane v1.2 Section 13] [1].

This Backplane v1.2 server implementation is backwards compatible with Backplane v1.1.
All endpoints are available at paths starting with `/v1.2/`, `/v1.1/` and `/v1/`.

### Get All

* Endpoint:  `/v1.2/bus/<BUS_NAME>`

* Security: HTTPS GET, HTTP-Basic authentication

* Request parameters: since (optional)

* **Request parameters: sticky (v1.2, optional)**

* Response body: list of backplane frames

### Get Channel

* Endpoint:  `/v1.2/bus/<BUS_NAME>/channel/<CHANNEL_NAME>`

* Security: HTTPS GET, no authentication

* Request parameters: since (optional)

* **Request parameters: sticky (v1.2, optional)**

* Response body: list of backplane frames

### Post

* Endpoint:  `/v1.1/bus/<BUS_NAME>/channel/<CHANNEL_NAME>`

* Security: HTTPS POST, HTTP-Basic authentication

* Request body: list of backplane messages

* Response body: empty on success or if the post exceeds the maximum number of message allowed
(default of 100 per channel) an error is returned; this value may be overridden by placing an
entry in the `<backplane-instance>_bpserverconfig` table called `DEFAULT_MESSAGES_MAX`
with an appropriate value.

```json
{"ERR_MSG":"Message limit exceeded for this channel"}
```


Configuration
=============

All Bus Provisioning API calls are only available over HTTPS.

Bus Configuration
-----------------

A (customer) bus is defined by the following Bus Configuration data, kept in the `<backplane-instance>_BusConfig1` table:

* `BUS_NAME`: bus name

* `RETENTION_TIME_SECONDS`: message retention time (seconds)

* `RETENTION_STICKY_TIME_SECONDS`: message retention time for sticky messages (seconds)

* `<backplane_user>:` comma-separated list of permissions

The defined set of permissions is:

* `GETALL`: allows access to the "Get All" API endpoint

* `POST`: allows posting of (common) messages to the bus

Note: An access decision for the "Post" operation requires the message type
(from which the required permission can be determined), in addition to the endpoint
(bus and channel) and credentials required for the "Get All" operation.


User Provisioning API
---------------------

#### Create or Update User Entry

Example HTTP API request:

POST `/v1.2/provision/user/update`
Host: backplanesample.com
Content-Type: application/json

```json
{
  "admin": "admin",
  "secret": "admin",
  "configs": [
    {
        "USER": "user1",
        "PWDHASH": "user1"
    },
    {
        "USER": "user2",
        "PWDHASH": "user2"
    }
  ]
}
```

Example curl command for the above HTTP API request:

    curl -i --data @json_data_file.txt -H "Content-type: application/json" http://backplanesample.com/v1.1/provision/user/update


#### List All Users

Example HTTP API request:

POST `/v1.2/provision/user/list`
Host: backplanesample.com
Content-Type: application/json

```json
{
  "admin": "admin",
  "secret": "admin",
  "entities": []
}
```

Example curl command for the above HTTP API request:

    curl -i --data @json_data_file.txt -H "Content-type: application/json" http://backplanesample.com/v1.1/provision/user/list

#### Delete User(s)

Example HTTP API request:

POST `/v1.2/provision/user/delete`
Host: backplanesample.com
`Content-Type: application/json`

```json
{
  "admin": "admin",
  "secret": "admin",
  "entities": [ "user1", "user2" ]
}
```

Example curl command for the above HTTP API request:

    curl -i --data @json_data_file.txt -H "Content-type: application/json" http://backplanesample.com/v1.1/provision/user/delete



Bus Provisioning API
--------------------

#### List Buses

Returns a list of Bus Configuration data sets.

Request:

* endpoint: `/v1.2/provision/bus/list`
* HTTP method: POST
* all entries are required
* the value for the entities entry can be empty, in which case all Backplane Bus configurations are returned
* body format:

```json
{
    "admin": "<adminUsername>",
    "secret": "<adminPassword>",
    "entities": [ "customer1.com", "customer2.net" ]
}
```

Response:

* status 200 for requests with valid format that were processed successfully
* requested bus configurations that were not found will result in a `"ERR_MSG": "NOT_FOUND"` entry
* requested bus configurations that encountered retrieval errors will result in a `"ERR_MSG": "<error_message>"` entry
* body format:

```json
{
    "customer1":
        {
            "BUS_NAME": "customer1",
            "RETENTION_TIME_SECONDS": "600",
            "<username1>": "<permission1>,<permission2>,...",
            "<username2>": "<permission3>,<permission4>,...",
            ...
        },
...
}
```

    curl -i --data @json_data_file.txt -H "Content-type: application/json" http://backplanesample.com/v1.1/provision/bus/list

#### Delete Bus

Removes the specified bus configuration(s).

Request:

* endpoint: `/v1.2/provision/bus/delete`
* HTTP method: POST
* all entries are required
* body format:

```json
{
    "admin": "<adminUsername>",
    "secret": "<adminPassword>",
    "entities": [ "customer1.com", "customer2.net" ]
}
```

Response:

* status 200 for requests with valid format that were processed successfully
* an entry is returned for each bus configuration with the status of the delete operation - either "`BACKPLANE_DELETE_SUCCESS`" or an error message if the deletion was not completed successfully
* body format:

```json
{
    "customer1.com": "BACKPLANE_DELETE_SUCCESS",
    "customer2.net": "<delete error message>"
}
```

    curl -i --data @json_data_file.txt -H "Content-type: application/json" http://backplanesample.com/v1.1/provision/bus/delete

*NOTE:* Deleting a bus configuration removes all Privileged Access Level defined for it; it does *NOT* remove any existing Backplane Messages from any of its channels. They will still be available for retrieval through the "Get Channel Messages" Backplane operation until they expire.

#### Update Buses

Updates Bus configuration data. If a bus in the provided list does not exist, it is added to the Bus Configuration table.

Request (v1.1):

* endpoint: `/v1/provision/bus/update`
* HTTP method: POST
* all entries are required
* provided Bus configurations must be in the valid format for the specified endpoint type
* body format:

```json
{
    "admin": "<adminUsername>",
    "secret": "<adminPassword>",
    "configs": [
        {
            "BUS_NAME": "customer1",
            "RETENTION_TIME_SECONDS": "600",
            "<username1>": "<permission1>,<permission2>,...",
            "<username2>": "<permission3>,<permission4>,...",
            ...
            },
            ...
    ]
}
```

Request (v1.2):

* endpoint: `/v1.2/provision/bus/update`
* HTTP method: POST
* all entries are required
* provided Bus configurations must be in the valid format for the specified endpoint type
* body format:

```json
{
    "admin": "<adminUsername>",
    "secret": "<adminPassword>",
    "configs": [
        {
            "BUS_NAME": "customer1",
            "RETENTION_TIME_SECONDS": "600",
            "RETENTION_STICKY_TIME_SECONDS": "28800",
            "<username1>": "<permission1>,<permission2>,...",
            "<username2>": "<permission3>,<permission4>,...",
            ...
            },
            ...
    ]
}
```

Response / success:

* status 200 for requests with valid format that were processed successfully
* an entry is returned for each bus configuration with the status of the update operation - either `"BACKPLANE_UPDATE_SUCCESS"` or an error message if the deletion was not completed successfully.
* body format:

```json
{
    "customer1.com": "BACKPLANE_UPDATE_SUCCESS",
    "customer2.net": "<update error message>"
}
```

#### Retrieve Metrics

Backplane server publishes usages statistics.  Interesting data points include the number of channel gets,
posts and how long it takes the server to retrieve a payload.  Metrics exist in memory only, so if the node
is restarted, the metric data will be reset.

Request:

* Endpoint: `/backplane_metrics`
* HTTP method: GET
* Security: access restricted to localhost and white listed IPs

Metric data may optionally be pushed into Graphite.
Metric data will periodically be sent to stdout.
To configure white-listed IPs and Graphite server settings, please see the build instructions.


### Error Responses

status 401 Unauthorized - API authentication failure
status 400 Bad Request - invalid request format / syntax

Each error response body will contain an error message in the following format:

```json
{
    "ERR_MSG": "<message>"
}
```

[1]: http://www.backplaneworkinggroup.org/documentation/backplane1-2#backplane.server.api "Backplane v1.1 Section 13"
