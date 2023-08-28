defmodule Mix.Tasks.Compile.Neotoma do
  use Mix.Task
  alias Mix.Compilers.Erlang

  @recursive true
  @manifest ".compile.peg"

  @moduledoc """
  Compile peg source files using Neotoma.

  When this task runs, it will check the modification time of every file, and
  if it has changed, the file will be compiled. Files will be
  compiled in the same source directory with a .erl extension.
  You can force compilation regardless of modification times by passing
  the `--force` option.

  ## Command line options

    * `--force` - forces compilation regardless of modification times

  ## Configuration

    * `:erlc_paths` - directories to find source files. Defaults to `["src"]`.

    * `:neotoma_options` - compilation options that apply
      to Neooma's PEG compiler. There are many other available
      options here: http://www.erlang.org/doc/man/yecc.html#file-1.

  """

  @doc """
  Runs this task.
  """
  def run(args) do
    {opts, _, _} = OptionParser.parse(args, switches: [force: :boolean])

    project      = Mix.Project.config
    source_paths = project[:erlc_paths]
    mappings     = Enum.zip(source_paths, source_paths)
    options      = project[:neotoma_options] || []

    Erlang.compile(manifest(), mappings, :peg, :erl, opts[:force], fn
      input, output ->
        # Neotoma accepts the output dir, not the output filename
        options = options ++ [output: Path.dirname(output)]

        # Neotoma's file\1,2 functions' return value is adjusted for Erlang.compile/6
        case :neotoma.file(Erlang.to_erl_file(input), options) do
          :ok ->
            {:ok, Path.basename(input, ".peg")}
          {:error, _} ->
            :error
        end
    end)
  end

  @doc """
  Returns Peg manifests.
  """
  def manifests, do: [manifest]
  defp manifest, do: Path.join(Mix.Project.manifest_path, @manifest)

  @doc """
  Cleans up compilation artifacts.
  """
  def clean do
    Erlang.clean(manifest())
  end
end
