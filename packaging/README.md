Propadata (Propagate Data)
==========================


Overview
--------

The main goal of this project is to provide a way to keep some quasi-static data in memory of local erlang node and
easily replace/update it when needed.

In-memory data store gives you responsive access to data typically queried from a database. You would want to do this for various reasons.
 - You want speed
 - You want to remove the overhead of a database connection
 - You want to allow servers handling this data to be able to work alone
 - For example, a network partition or failure of the database does not affect the server

Packaging give's you a convenient way to do the following:
 - Convert source data of arbitrary format using or writing a small parser
 - Specify your own function (or use existent) to apply the data on production (you can choose to replace / update / whatever)
 - Specify event handlers for such events as "fail" and "success"
 - Change template beam (template is a module representing your data on production)

Packaging is performed by the converter.es script on an auxiliary (not production) server. Change and load is done by beampkg module [1] working on production.

Converter.es creates a package with instructions for updating or replacing data on production.
Beambag works on production waiting for new packages and atomically updating target module.
Packages (one file for a package) are deployed to production with any synchronization scheme (i.e. rsync).

    [1]: https://github.com/mochi/beambag


Propadata
---------

The core is converter.es erlang script. It has usage info (run without arguments).
Converter creates package which is compressed ETF of list of following format:

<pre>
[
    {data, ArbitraryData},
    {template, BeamBinary},
    {code_change, FunctionOf2Args} % optional
]
</pre>

Data is created by calling a parser on a source.
Source is a file of arbitrary input data.
Parser is a file (specified without .erl extension) containing an anonymous function accepting binary content of source and returning ArbitraryData.

Template is a file containing beam with target module.

Code_change is a file (specified without .erl extension) containing an anonymous function accepting target module name (atom) and new data (ArbitraryData).
Code_change can replace existing data or update it by requesting current state from target module.

It's strictly required to have the same version of erlang runtime working with propadata and beampkg.


Beambag
-------

Beambag is a gen server that installs packages.

When beampkg starts it trys to load the last compiled state of the module.
If there is a new package it trys to install it.

In case of problem during installation of any package the previous version ( if there is any ) keeps working.

After starting, beampkg keeps checking for fresher packages by looking into PackageDir with PackageWildcard specified at start. It checks for newer versions every 5 seconds.

If it finds a new version, it checks the package's integrity (comparing md5 sum), loads it, loads the new template from it, calls the code_change function (if there is such) with the new data, replaces {'$$magic'} in template with new data and changes the target module code.

New compiled code is saved on the local filesystem and is loaded if beampkg server or runtime is restarted.

It's the duty of devops to purge old package files after loading them. If there are more than 10 packages on PackageDir beampkg starts reporting this via the log. It doesn't mean that something isn't working.

It's strictly required to have the same version of erlang runtime working with propadata and beampkg.


Example
-------

Suppose you have some data in CSV format and you want to have it accessible on production as a dict (first field is a key and other are a value as tuple).

First of all, you need to have a template module. You will use it to access your data on production.
It could be very simple:

<pre>
$ cat simple_template.erl
-module(simple_template).
-export([data/0]).

data() -> {'$$magic'}.
$ erlc simple_template.erl
</pre>

Tuple of an atom with name '$$magic' is a special stub which will be replaced by actual data.

Second, you need a source data in CSV

<pre>
$ cat sortings.csv
mergesort,nlogn,nlogn,nlogn,n,stable
heapsort,nlogn,nlogn,nlogn,1,stable
quicksort,nlogn,nlogn,n2,logn,not_stable
</pre>

Luckily you already have parser for CSV, so you need not to write your own

<pre>
$ cat csv.erl
fun(Source) when is_binary(Source) ->
    Map = lists:map(fun(Line) ->
                      case binary:split(Line, <<",">>, [global]) of
                          [Key | Value] when Value =/= [] ->
                              {Key, list_to_tuple(Value)};
                          _ -> undefined
                      end
              end, binary:split(Source, <<"\n">>, [global])),
    lists:filter(fun(undefined) -> false; (_) -> true end, Map)
end.
</pre>

And you would like to update your current dictionary with this new data if there is any, so you need your own code_change function

<pre>
$ cat update_dict.erl
fun(Module, NewData) ->
    ExistentDict = try Module:data() catch _:_ -> dict:new() end,
    dict:merge(fun(_, _, NewValue) -> NewValue end, ExistentDict, dict:from_list(NewData))
end.
</pre>

So you have everything you need and can set up deployment.

First, you should check that erlang runtime on your auxiliary server and on production is of the same version.
Then you can generate a package:

<pre>
$ cd /auxiliary_beampkg_packages/ && converter.es source=sortings.csv parser=csv template=simple_template.beam code_change=update_dict
propadata.67699e588407ecdc187378d80349c3ac
</pre>

The propadata.67699e588407ecdc187378d80349c3ac file is the package. Hex number in the name is a MD5 checksum for integrity checking.

Let's prepare the production side now.
To simplify example we don't use supervisor.
First, let's start beampkg:

<pre>
1> beampkg:start_link(simple_template, "/production_beampkg_packages", "propadata.[0-9a-f]*").
<0.12.0>
</pre>

If there was any package installed before beampkg will load the last state.

The only thing left is to set up syncing of packages from auxiliary server and production.
And also we would like to delete old (from 6th and further) packages as well.

<pre>
$ ls -t1 /auxiliary_beampkg_packages/propadata.[0-9a-f]* | tail -n +6 | xargs rm -f
$ rsync -av --delete /auxiliary_beampkg_packages/ production:/production_beampkg_packages/
</pre>

That's it. The new package will be copied to production and after a short time (~ 5 seconds) will be read and installed there.
