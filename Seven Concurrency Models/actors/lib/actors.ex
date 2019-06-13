defmodule Actors do
  def talker do
    Process.flag(:trap_exit, true)

    pid = spawn_link(&Talker.loop/0)
    send(pid, {:greet, "Marcos"})
    send(pid, {:celebrate, "Marcos", 42})
    send(pid, {:shutdown})

    receive do
      {:EXIT, ^pid, reason} ->
        IO.puts("Talker has exited (#{reason})")
    end
  end
end
