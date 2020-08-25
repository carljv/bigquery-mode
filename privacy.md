# `bigquery-el` Terms of Service and Privacy Policy

## Terms of Service

Your use of Google APIs with this package is subject to each API’s respective terms of service. See https://developers.google.com/terms/.

## Privacy

### Accessing user data

This package accesses Google resources from your local machine. Your machine communicates directly with the Google API.

The `biquery-el` package never receives your data or the permission to access your data. The owners of the package can only see anonymous, aggregated information about usage of tokens obtained through its OAuth client, such as which APIs and endpoints are being used.

The package includes functions that you can execute in order to read or modify your own data. This can only happen after you provide a token, which requires that you authenticate yourself as a specific Google user and authorize these actions.

The package can help you get a token by guiding you through the OAuth flow in the browser. There you must consent to allow the `bigquery-el` package to operate on your behalf. The OAuth consent screen will describe the scope of what is being authorized, e.g., it will name the target API(s) and whether you are authorizing “read only” or “read and write” access.


### Scopes

The `bigquery-el` package lets you query data stored in Google BigQuery, as well as retrieve metadata about projects, datasets, tables, and jobs.
   
   
### Sharing user data

The `bigquery-el` package only communicates with Google APIs. No user data is shared with the owners of the package or any other service.

### Storing user data

The `bigquery-el` package may store your credentials on your local machine, for later reuse by you. Use caution when using the package on a shared machine.

By default, an OAuth token is cached in a local file, such as `~/.emacs.d/bigquery-el/.bigquery-oauth`. See the package documentation for for information on how to control the location of the token cache or suppress token caching.
