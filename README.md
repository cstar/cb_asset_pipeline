# Asset Pipeline for Chicago Boss

This document is a work in progress and the code is not finished yet.

## What does it do ?

[Chicago Boss](http://chicagoboss.org) is a fantastic framework. This project attempts to add an asset pipeline à la Rails 3.1.

It will minify, concatenate, statically compress your assets.

These assets are served with a unique name (based on the content md5 hash) and set with far away Expires headers.

This CB application is a supporting application, meant to be installed alongside your application.

It will serve assets from the `priv/assets/javascript/` directory of the app specified in the `assets_for` param in boss.config.

For embbeding, add the following to your application boss.config file along with your app:

```erlang
{ asset_pipeline, [
  {base_url, "/assets"},
  {assets_for, <YOUR APP>},
  {concatenate, true}, % Values are true, false, and production (production is the default)
  {minify, true}, % Values are true, false, and production (production is the default)
  {dummy, true}
  ]}
```

Don't forget to add asset_pipeline to the list of applications Chicago Boss should start :

    [{boss, [
      {path, "<path to boss>"},
      {applications, [<your app>, cb_admin, asset_pipeline]},

Finally add the asset tags to your custom tags (in `src/view/lib/tag_modules/<app>_custom_tags.erl`) :

```erlang
javascript(Variables, Options) ->
  asset_pipeline_custom_tags:javascript(Variables, Options).
```

(I'm aware this is not as elegant as it should be. Will try and find a workaround.)

In your view :

```dtl
{% javascript src="application.js" %}
```

In your JS files, require other JS files :

```javascript
// = require eventsource.js
// = require rails.js
// = require run.js
```

`require_tree` is not supported at this time.

Start your app, and make [Steve Souders](http://www.stevesouders.com) proud!

## Current state of affairs

Currently only works with JS files. CSS support is coming very soon.

The implementation is very naive at this time. Feedback valued and encouraged.

## Try me quick.

Clone the repo, edit boss.config — I'm pretty sure you'll need to fix the Chicago Boss path.

Start the server :

    init-dev.sh


And visit [http://localhost:8001/assets/home](http://localhost:8001/assets/home).

In your favorite browser inspector, you'll see that the javascript is served first with a `.raw.js` extension without caching headers. But this first request will trigger compression, concatenation and minification, so on the next reload, you'll have `application.a8236b610331fc51b5fec62474325970.js` served, with the correct headers.

Included is also the asset watcher which will automatically reload assets as files are modified.

## Future plans

Add support for stylesheets and images.

Add `require_tree` and other unsupported keywords.

Pass assets through the DTL compiler

Add support for less, sass and coffeescript.

Automatically deploy assets to S3/CloudFront and serve them transparently.


## Author

[Eric Cestari](https://twitter.com/cstar) 

The JS minification code included is not mine and was lifted from [ztmr's asset compressor](https://github.com/ztmr/cb_asset_compressor)

## License

Copyright (c) 2013 Eric Cestari (except where otherwise noted)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
