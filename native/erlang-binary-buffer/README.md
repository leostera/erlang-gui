# Mutable Binary Buffer for the BEAM

This is a very tiny module used as an experiment on opaque
resource types for the BEAM.

It is:

* safe - since it won't crash the BEAM),
* unsafe - since it doesn't perform boundary checks
* fast! - since its a mutable `Vec<u8>` under the hood
