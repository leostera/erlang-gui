defmodule Skia.Native do
  use Rustler, otp_app: :skia, crate: :skia

  def draw(), do: :erlang.nif_error(:nif_not_loaded)
end
