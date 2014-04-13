package 'curl'

execute 'add-basho-repository' do
	command <<-EOF
		curl http://apt.basho.com/gpg/basho.apt.key | sudo apt-key add -
		bash -c "echo deb http://apt.basho.com $(lsb_release -sc) main > /etc/apt/sources.list.d/basho.list"
		apt-get update
	EOF
end

package 'riak'

service 'riak' do
	supports :start => true, :stop => true, :status => true
	action :start
end