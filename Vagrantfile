# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

$install = <<INSTALL
apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv D208507CA14F4FCA
echo "deb http://packages.erlang-solutions.com/debian precise contrib" > /etc/apt/sources.list.d/erlang-solutions.list
curl -sL https://deb.nodesource.com/setup | sudo bash -
apt-get -q update
apt-get -y -q install erlang git gettext nodejs libmagic-dev
apt-get -q clean
echo "VSSRTIKSRMKWXYPDLJKX" > /home/vagrant/.erlang.cookie
chmod 0400 /home/vagrant/.erlang.cookie
chown vagrant:vagrant /home/vagrant/.erlang.cookie
sudo npm install -g bower less
INSTALL

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "Ubuntu"
  config.vm.box_url = "https://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box"
  config.vm.provision "shell", inline: $install
  config.vm.synced_folder ".", "/home/vagrant/src"

  config.vm.define "node1" do |node1|
      node1.vm.network "private_network", ip: "192.168.50.2"
      node1.vm.network :forwarded_port, host: 8000, guest: 8080
  end

  config.vm.define "node2" do |node2|
      node2.vm.network "private_network", ip: "192.168.50.3"
      node2.vm.network :forwarded_port, host: 8001, guest: 8080
  end

  config.vm.define "node3" do |node3|
      node3.vm.network "private_network", ip: "192.168.50.4"
      node3.vm.network :forwarded_port, host: 8002, guest: 8080
  end

  config.vm.provision :hosts do |provisioner|
    provisioner.add_host "192.168.50.2", ["node1"]
    provisioner.add_host "192.168.50.3", ["node2"]
    provisioner.add_host "192.168.50.4", ["node3"]
  end

end
