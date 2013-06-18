# Asset Compiler for Chicago Boss

This document is a work in progress and the code is not finished yet.

## What does it do ?

It will minify, concatenate, statically compress your assets.

These assets are served with a unique name (based on the content md5 hash) and set with far away Expires headers.

This CB application is a supporting application, meant to be installed alongside your application.

It will serve assets from the `priv/assets` directory of the app specified in the `assets_for` param in boss.config.

For embbeding, add the following to your application boss.config file :

    { asset_pipeline, [
        {path, "../asset_pipeline"},
        {base_url, "/assets"},
        {assets_for, <YOUR APP>},
        {concatenate, true}, % Values are true, false, and production (production is the default)
        {minify, true}, % Values are true, false, and production (production is the default)
        {dummy, true}
    ]}.

TODO (embedding instructions are not finished yet)

## Current state of affairs

Currently only works with JS files. CSS are coming very soon.

## Try me

Clone the repo, edit boss.config, I'm pretty sure you'll need to fix the Chicago Boss path.

    init-dev.sh


And visit [http://localhost:8001/assets/home](http://localhost:8001/assets/home).

In your favorite browser inspector, you'll see that the javascript is served first with a `.raw.js` extension without caching headers. But this first request will trigger compression, concatenation and minification, so on the next reload, you'll have `application.a8236b610331fc51b5fec62474325970.js` served, with the correct headers.

Included is also the asset watcher which will reload assets as files are modified.