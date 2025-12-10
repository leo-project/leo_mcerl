# leo_mcerl

leo_mcerl is a memory cache library for Erlang, providing efficient in-memory caching with LRU (Least Recently Used) eviction.

## Requirements

- Erlang/OTP 22 or later (tested up to OTP 28)
- rebar3
- cmake
- C compiler (gcc or clang)

### Build Dependencies

On macOS:
```bash
brew install cmake check
```

On Ubuntu/Debian:
```bash
sudo apt-get install cmake check
```

## Build

```bash
make compile
```

## Test

```bash
make eunit
```

## Usage

```erlang
%% Start the application
application:start(leo_mcerl).

%% Start a cache server with 1MB capacity
{ok, Pid} = leo_mcerl_sup:start_child(my_cache, 1024 * 1024).

%% Store a value
ok = leo_mcerl_server:put(my_cache, <<"key">>, <<"value">>).

%% Retrieve a value
{ok, <<"value">>} = leo_mcerl_server:get(my_cache, <<"key">>).

%% Delete a value
ok = leo_mcerl_server:delete(my_cache, <<"key">>).

%% Get cache statistics
{ok, Stats} = leo_mcerl_server:stats(my_cache).

%% Get cache size
{ok, Size} = leo_mcerl_server:size(my_cache).

%% Get number of items
{ok, Items} = leo_mcerl_server:items(my_cache).

%% Stop the cache server
leo_mcerl_server:stop(my_cache).
```

## License

leo_mcerl's license is [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html)
