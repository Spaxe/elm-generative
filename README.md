[**View the demo online**](https://spaxe.github.io/elm-generative/)

# Generative Art with Elm

This package is a set of utilities for creating random, geneative art.

Optionally, this website also can plot directly to [AxiDraw V3](https://www.axidraw.com/) and related penplotter families using the [AxiDraw CLI](https://axidraw.com/doc/cli_api/#introduction).

## Getting started

1.  [Follow the Elm Guide to install Elm Platform](https://guide.elm-lang.org/install.html).
2.  Run `elm reactor`
3.  Click on `index.html` to see the web demo

### Optional, if you have an AxiDraw penplotter physically
4.  Install the [AxiDraw Python Module](https://axidraw.com/doc/cli_api/#installation)
5.  In a new terminal, install python dependencies: `pip -r python/requirements.txt`
6.  Run `python python/axidraw_server.py`

If the website shows the AxiDraw firmware version, that means it's connected

## Branching strategy

Releases go on `master` with tags. All features must be merged via a pull request.

## Recommended resources

- Elm - [Introduction to Elm](https://guide.elm-lang.org/)
- Elm - [Design Guidelines](http://package.elm-lang.org/help/design-guidelines)

## Contribute

You can contribute to this repository:

- Open a new issue and make suggestions
- Create a pull request with new features or bug fixes
- Tweet to me: [@xavier_ho](https://twitter.com/xavier_ho)

---

Made with an open mind by Xavier Ho <xavier@jtg.design>
