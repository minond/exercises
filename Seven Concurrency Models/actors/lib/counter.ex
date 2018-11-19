defmodule Counter do
  def start(count) do
    spawn(__MODULE__, :loop, [count])
  end

  def next(counter) do
    ref = make_ref()
    send(counter, {:next, self(), ref})

    receive do
      {:ok, ^ref, count} -> count
    end
  end

  def loop(count) do
    receive do
      {:next, sender, ref} ->
        send(sender, {:ok, ref, count})
        loop(count + 1)
    end
  end
end
