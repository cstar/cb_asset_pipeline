# Asset Compiler for Chicago Boss

This document is a work in progress.

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



## Current state of affairs

Currently only works with JS files. CSS are coming very soon.

## Try me

Clone the repo, edit boss.config, I'm pretty sure you'll need to fix the Chicago Boss path.

    init-dev.sh


And visit http://localhost:8001/assets/home