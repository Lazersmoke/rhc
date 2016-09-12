class Pretty p where
  pretty :: p -> String

instance Pretty Integer where
  pretty = show
instance Pretty Double where
  pretty = show

