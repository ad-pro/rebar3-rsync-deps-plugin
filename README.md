# rebar3-rsync-deps-plugin
A rebar3 plugin to enable fetching dependencies using file copy functionality (rsync).


## Description

An old version (rebar2) supported rsync as a dependency source,
unfortunately  rebar3 only allows to use on-line repositories (git, hg, hex3 at the moment).

This plugin us rsync to control dependencies.

To optimize work it try to calculate md5 sum of the folder content.
Following bash command is used:

```
  `find <Dir> -type f -exec md5sum {} + | sort | md5sum`
```
If this failed,  the UUID is used to generate reference (this could be helpful under Win platform).

Plugin inspired by: https://github.com/kellymclaughlin/rebar3-tidy-deps-plugin

## Suported OS
 
 Plugin initialy tested on Linux.

 To run it under windows Cygwin is required.

 NOTE: check that your cygwin path is in front of Windows.
 Plugin use cygwin find command and would not work with windows find.

## Configuration

Configure the plugin and and set it as a pre hook for the
`app_discovery` and `install_deps` providers by adding the following
to the rebar.config file:

```
{plugins, [
    {rebar_rsync_deps, ".*", {git, "https://github.com/ad-pro/rebar3-rsync-deps-plugin.git",{branch,"master"}}}
]}.
```

## Example

Assume that files you want to add to your project located at
```
  ../my-deps/project-1
```
folder.

Add following line to your deps specification in rebar.config

```
{deps, [
        {rsync, {rsync, "../my-deps/project-1", {branch, "master"}}}
       ]}.
```

rsync ingore  branch/tag specification.


## Examples from https://github.com/kellymclaughlin/rebar3-tidy-deps-plugin

If you are using a test profile to pull in test-only depencencies then
the same conversion applies.

This test profile dep specification:

```
{profiles, [
            {test, [
                    {deps, [
                            {meck, ".*", {git, "https://github.com/eproxus/meck.git", {tag, "0.8.2"}}}
                           ]}
                   ]}
           ]}.
```

becomes this:

```
{profiles, [
            {test, [
                    {deps, [
                            {meck, {github, "eproxus/meck.git", {tag, "0.8.2"}}}
                           ]}
                   ]}
           ]}.
```
