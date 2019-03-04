# nvim-hs-lsp

[Language Server Protcol](https://microsoft.github.io/language-server-protocol/specification) implementation for neovim using [nvim-hs](https://github.com/neovimhaskell/nvim-hs).

## Installation

1. Install [stack](https://docs.haskellstack.org/en/stable/README/)
2. Install [nvim-hs.vim](https://github.com/neovimhaskell/nvim-hs.vim) and this plugin

    If you are using vim-plug:

    ```vim
    Plug 'neovimhaskell/nvim-hs.vim'
    Plug 'Hogeyama/nvim-hs-lsp'
    ```

    Note: It takes quite a long time to install.

## Screen shots

### Code action

<img src="./screenshot/CodeAction.gif" width="600">

### Jump to definition

<img src="./screenshot/Definition.gif" width="600">

### Completion

+ `completefunc`
+ [`deoplete`](https://github.com/Shougo/deoplete.nvim) source

<img src="./screenshot/Completion.gif" width="600">


## Technical challenge in implementation

### More Precise definition of the protocol

The [specification](https://microsoft.github.io/language-server-protocol/specification) of Language Server Protocol is written in Type Script, the type system of which has quite strong expressive power.
For example, `FormattingOptions` is defined as follows:

```typescript
interface FormattingOptions {
  tabSize: number;
  insertSpaces: boolean;
  [key: string]: boolean | number | string;
}
```

This interface means that an object `obj` is `FormattingOptions` if and only if

+ `obj` contains `'tabSize'` field of type `number`,
+ `obj` contains `'insertSpaces'` field of type `boolean`, and
+ any other field of `obj` has type of either `boolean`, `number`, or `string`.

For example, `{ 'tabSize' : 2, 'insertSpaces' : true, 'foo' : 42 }` is a `FormattingOptions` but
`{ 'tabSize' : 2, 'foo' : 42 }` and `{ 'tabSize' : 2, 'insertSpaces' : true, 'foo' : [] }` are not.

This property cannot be expressed by usual haskell records.
In fact, the library [`haskell-lsp-types`](https://hackage.haskell.org/package/haskell-lsp-types)
gives up precise definition:

```haskell
data FormattingOptions =
  FormattingOptions
    { _tabSize      :: Int
    , _insertSpaces :: Bool -- ^ Prefer spaces over tabs
    -- Note: May be more properties
    } deriving (Read,Show,Eq)
```

But in `nvim-hs-lsp`, `FormattingOptions` is precisely defined with the help of [`extensible`](https://hackage.haskell.org/package/extensible) library:

```haskell
type family FOField' x :: Constraint where
  FOField' Number = ()
  FOField' Bool = ()
  FOField' String = ()
  FOField' x = TypeError ('Text "Unexpected Type for FormattingOptions: " ':<>: 'ShowType x)
class FOField' x => FOField x
instance FOField' x => FOField x

data FormattingOptions where
  FormattingOptions
    :: forall xs.
          ( Associate "tabSize" Number xs
          , Associate "insertSpaces" Bool xs
          , Forall (KeyValue KnownSymbol FOField) xs
          , Eq (Record xs)
          , Show (Record xs)
          , ToJSON (Record xs)
          , FromJSON (Record xs)
          )
    => Record xs -> FormattingOptions
```

Examples:


```haskell
>>> :{
>>> let good = FormattingOptions $ Record $
>>>                #tabSize @= 2
>>>             <! #insertSpaces @= True
>>>             <! #foo @= (42 :: Number)
>>>             <! nil
>>> :} -- successfully defined
>>> :{
>>> let bad1 = FormattingOptions $ Record $
>>>                #tabSize @= 2
>>>             <! #foo @= (42 :: Number)
>>>             <! nil
>>> :}
<interactive>:10:12: error:
    • Couldn't match type ‘'Missing "insertSpaces"’
                     with ‘'Expecting (n0 ':> Bool)’
        arising from a use of ‘FormattingOptions’
>>> :{
>>> let bad2 = FormattingOptions $ Record $
>>>            #tabSize @= 2
>>>         <! #insertSpaces @= True
>>>         <! #foo @= ([] :: [Int])
>>>         <! nil
>>> :}
<interactive>:10:12: error:
    • Unexpected Type for FormattingOptions: [Int]
```

### Less boilerplate

I also made the use of `extensible` library to reduce boilerplates.
In `haskell-lsp-types`, again, many boilerplates is used to make lenses or `ToJSON`/`FromJSON` instances (see [this file][haskell-lsp-types-lens] for example).
In `nvim-hs-lsp`, all lenses and `ToJSON`/`FromJSON` instances are defined at once in [this file][nvim-hs-lsp-record].

[haskell-lsp-types-lens]: https://github.com/alanz/haskell-lsp/blob/master/haskell-lsp-types/src/Language/Haskell/LSP/Types/Lens.hs
[nvim-hs-lsp-record]: ./src/LSP/Record.hs

