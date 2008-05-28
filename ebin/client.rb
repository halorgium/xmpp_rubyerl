require 'rubygems'
require 'erlectricity'
require 'xmpp4r-simple'
require 'yaml'
require 'logger'

require 'cgi'

Jabber::debug = true

class Notifier
  def initialize(config)
    @config = config
  end
  attr_reader :config

  def client
    @client ||= connect
  end

  def connect
    client = Jabber::Simple.new(jid, pass)
    client.status(:chat, "assisting you")
    client
  end

  def disconnect
    client.disconnect if client
  end

  def profile
    config["profile"]
  end

  def recipients
    config["recipients"]
  end

  def jid
    "#{profile["jid"]}/#{profile["resource"]}"
  end

  def pass
    profile["pass"]
  end

  def deliver(message)
    message.chomp!
    message.gsub!(/[\000-\010\013-\037\177-\377]/, "?")
    LOGGER.info "Delivering #{message}"
    recipients.each do |recipient|
      client.deliver(recipient, message)
    end
  rescue
    LOGGER.error $!.message
    LOGGER.error $!.backtrace
    raise $!
  end

  def loop
    receive do |f|
      f.when(:deliver, Any) do |message|
        deliver message
        f.receive_loop
      end

      f.when(Any) do |*objs|
        $stderr.puts "objs: #{objs.inspect}"
      end
    end
  end
end

LOGGER = Logger.new(File.dirname(__FILE__) + "/../priv/client.log")
def $stderr.write(content)
  content.chomp!
  return if content.empty?
  LOGGER.error "stderr: #{content}"
end

config = YAML.load_file(File.dirname(__FILE__) + "/../priv/config.yml")
begin
  notifier = Notifier.new(config)
  notifier.deliver("Started up!")
  notifier.loop
rescue
  LOGGER.error "Crashing out!!!"
  sleep 2
ensure
  notifier.disconnect
end

LOGGER.info "Shutdown!"
