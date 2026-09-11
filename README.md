# shinySCP

![Static Badge](https://img.shields.io/badge/demo-rshiny_app-%23be7e8d?style=flat&logo=R&logoColor=white&labelColor=%238a5f73)


shinySCP is a Shiny app for browsing files on a remote server over SSH. The
project takes inspiration from graphical SCP clients such as WinSCP and
FileZilla.

## Why shinySCP?

This is just a way for me to learn more about shiny and see one of my ideas come to life.

## Setup & Requirements

Restore the project library from the lockfile:

```r
install.packages("renv")
renv::restore()
```

Install the optional development and deployment tools when you need to run
package checks or regenerate `manifest.json`:

```r
renv::install(c("testthat", "devtools", "rsconnect"))
```

## Usage

The public app runs in demo-only mode. Select **Try Demo** to explore a
fictional, read-only HPC-style filesystem without entering credentials or
opening a network connection. The demo includes projects, analysis outputs,
scripts, container images, and hidden configuration entries. Select **Show
hidden files** to reveal the hidden entries.

For a trusted private deployment, enable real SSH login before starting the app:

```r
Sys.setenv(SHINYSCP_ENABLE_SSH_LOGIN = "true")
shiny::runApp(".")
```

When enabled, passwords are used for the connection attempt, cleared from the
browser afterward, and not stored in the app's reactive session state.

### Deployment security

Keep SSH login disabled for unrestricted public deployments. When SSH login is
enabled, the deployed instance initiates connections from its host. Protect the
instance with platform authentication and restrict destination servers to an
explicit allowlist.

## What's next for shinySCP?

The current implementation allows you to connect to a server, navigate remote
directories, refresh directory listings, and select a file or directory.

The next features will build on the remote file browser:

- [x] Connect to a remote server over SSH.
- [x] Browse remote directories and select entries.
- [x] Explore a packaged demo filesystem without an SSH connection.
- [ ] Download one selected file via the [ssh](https://github.com/ropensci/ssh) package.
- [ ] Allow users to view contents of their text-based files.

## Author

Shaurita Hutchins · [@sdhutchins](https://github.com/sdhutchins) · [:email:](mailto:sdhutchins@outlook.com)

## License

[MIT](https://github.com/sdhutchins/shinySCP/blob/master/LICENSE)
