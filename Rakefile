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
