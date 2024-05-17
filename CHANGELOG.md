# 1.0.5

- Support GHC-8.6.5..GHC-9.10.1

# 1.0.4.1

- Support OneTuple-0.4

# 1.0.4

- Depend on `data-byte-array` to provide `Data.Array.Byte` instance

# 1.0.3

- Add `ByteArray` (from `Data.Array.Byte` instance)

# 1.0.2

- Add `Solo` instance

# 1.0.1

- Fix `MonadFail` instances shim
- `NonEmpty` doesn't fail on empty list

# 1

Stripped down the package to only shim `binary` orphans.

For more instances check [binary-instances](https://hackage.haskell.org/package/binary-instances) package.
