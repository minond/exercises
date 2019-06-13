defmodule Talker do
  def loop do
    receive do
      {:greet, name} ->
        IO.puts("Hello #{name}.")

      {:celebrate, name, age} ->
        IO.puts("Here's not another #{age} years, #{name}.")

      {:shutdown} ->
        exit(:normal)
    end

    loop()
  end
end
