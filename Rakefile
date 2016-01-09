Sisters = %w( utrennyaya vechernyaya polunochnaya )

desc "Deploy to all environments"
task :deploy do
  unless ENV["DOCKER_MACHINE_NAME"]
    puts "Docker machine does not appear to be set. Try:"
    puts "eval \"$(docker-machine env dev)\""
    exit 1
  end

  Sisters.each do |sister|
    system "heroku docker:release --app #{sister}"
  end
end

desc "Build an env string, suitable for `heroku config:set`"
task :env do
  env        = File.read File.expand_path "../.env", __FILE__
  fields     = env.gsub(/^export /, '').lines
  applicable = fields.reject { |f| f =~ /^DATABASE_URL/ }
  puts applicable.join(" ").gsub /\s+/, ' '
end
