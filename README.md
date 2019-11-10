# elm-egift

## developent

`$ npm install -g elm elm-live`

`$ elm-live src/Main.elm -u -hot` 

### mock API

`$ npm install -g json-server`

`$ json-server --watch mock/db.json`

Then mock API served at `localhost:3000`.
(notice: The structure of categories and designs are different from response provided by e-gifts, because of json-server implmentation.)
