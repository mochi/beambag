# Beambag basic index example

Beambag is used to provide an in-memory data store for responsive access to data
typically queried from a database. You would want to do this for various reasons.

* You want speed.
* You want to remove the overhead of a database connection.
* You want to allow servers handling this data to be able to work alone.
 * For example, a network partition or failure of the database does not affect the server.
* And many more



## Postgresql database