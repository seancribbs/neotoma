defmodule Neotoma.Mixfile do
  use Mix.Project

  @version File.read!("VERSION") |> String.strip

  def project do
    [app: :neotoma,
     version: @version,
     elixir: "~> 0.14",
     package: package,
     description: description
    ]
  end

  def application do
    [applications: []]
  end

  defp package do
    [files: ~w(src lib mix.exs priv rebar.config README.textile LICENSE VERSION),
     contributors: ["Sean Cribbs"],
     licenses: ["MIT License"],
     links: %{"GitHub" => "https://github.com/seancribbs/neotoma"}]
  end

  defp description do
    """
    Erlang library and packrat parser-generator for parsing expression grammars.
    """
  end

end