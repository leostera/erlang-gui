defmodule Winit.Native do
  use Rustler, otp_app: :winit, crate: :winit

  def new_event_loop(), do: :erlang.nif_error(:nif_not_loaded)
  def run_event_loop(_event_loop), do: :erlang.nif_error(:nif_not_loaded)

  def new_window(), do: :erlang.nif_error(:nif_not_loaded)
  def build_window(_event_loop), do: :erlang.nif_error(:nif_not_loaded)

end
