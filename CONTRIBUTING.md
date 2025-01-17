# Contributing to ottrpal

Thank you for your interest in contributing to ottrpal! We value the contributions of all community members and want to make the contribution process as smooth as possible.

## Ways to Contribute

There are several ways you can help improve ottrpal:

- Reporting bugs and submitting feature requests
- Improving documentation
- Contributing code changes
- Sharing feedback and ideas

## Getting Started

1. Fork the repository
2. Create a new branch for your contribution
3. Make your changes
4. Submit a pull request

## Development Guidelines

### Code Style

- Follow the [tidyverse style guide](https://style.tidyverse.org/) for R code
- Document all exported functions using roxygen2
- Include examples in function documentation
- Add unit tests for new functionality using testthat

### Pull Request Process

1. Create a descriptive branch name (e.g., `fix-parsing-bug` or `add-new-feature`)
2. Make focused commits with clear commit messages
3. Update documentation if needed
4. Add tests for new functionality
5. Ensure all tests pass locally
6. Push your changes and create a pull request
7. Respond to any feedback during code review

### Submitting Issues

When submitting an issue, please:

1. Check if a similar issue already exists
2. Include a clear title and description
3. Provide a minimal reproducible example if reporting a bug
4. Add relevant labels
5. Tag @cansavvy for visibility

Use issue templates when available and include:

- Steps to reproduce (for bugs)
- Expected behavior
- Actual behavior
- R session information (via `sessionInfo()`)
- Package version

## Code of Conduct

This project follows a Code of Conduct. By participating, you agree to uphold this code. Please report unacceptable behavior to @cansavvy.

## Getting Help

If you need help:

1. Check existing documentation
2. Search closed issues for similar problems
3. Open a new issue with your question
4. Tag @cansavvy in issues or pull requests for assistance

## Licensing

By contributing to ottrpal, you agree that your contributions will be licensed under the same terms as the project. See the LICENSE file for details.

## Development Environment Setup

1. Install R and RStudio
2. Install development dependencies:
```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```
3. Clone your fork of the repository
4. Open the project in RStudio
5. Install package dependencies:
```r
devtools::install_deps()
```

## Testing

You can run tests locally before filing a PR. OR when you file the PR these tests will be run for you: 
```r
devtools::test()
```
You can also check that your documentation builds properly: 
```r
devtools::document()
```

## Review Process

1. Maintainers will review pull requests
2. Changes may be requested
3. Once approved, changes will be merged
4. Contributors will try to respond to comments within a month

Thank you for contributing to ottrpal! Your efforts help make this package better for everyone.
