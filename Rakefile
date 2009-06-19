require 'rake/clean'
CLOBBER.include 'ebin'
CLOBBER.include 'ebin_tests'
SRC_TO_BIN = [/src(\/.*\.)erl/,'ebin\1beam']
TEST_TO_BIN = [/tests(\/.*\.)erl/,'ebin_tests\1beam']
BIN_TO_SRC = [/ebin(\/.*\.)beam/,'src\1erl']
BIN_TO_TEST = [/ebin_tests(\/.*\.)beam/,'tests\1erl']
ERL='erl'
ERLC='erlc'
ERLC_OPT='+debug_info -W -I include -pz ebin'

beams = ['ebin'] + FileList.new('src/*.erl').gsub(*SRC_TO_BIN)
test_files = ['ebin_tests'] + FileList.new('tests/*.erl').gsub(*TEST_TO_BIN)
test_examples = ['ebin_tests/examples'] + FileList.new('tests/examples/*.erl').gsub(*TEST_TO_BIN)
task :default => beams

directory 'ebin'
directory 'ebin_tests'
directory 'ebin_tests/examples'

file "src/peg_meta.erl" => "ebin/peg_transform.beam"

rule(%r{^ebin/.*\.beam$} => lambda { |fn| fn.gsub(*BIN_TO_SRC) }) do |t|
  sh "#{ERLC} #{ERLC_OPT} -o ebin #{t.source}"
end

rule(%r{^ebin_tests/.*\.beam$} => lambda { |fn| fn.gsub(*BIN_TO_TEST) }) do |t|
  target_dir = File.dirname(t.name)
  sh "#{ERLC} #{ERLC_OPT} -o #{target_dir} #{t.source}"
end

task :test => (beams + test_files + test_examples) do
  sh "#{ERL} -pz ebin -pz ebin_tests -pz ebin_tests/examples -b start_sasl -noshell -s init stop -eval 'test_suite:test().'"
end
