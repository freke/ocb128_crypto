# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"
ENV['VAGRANT_DEFAULT_PROVIDER'] ||= 'docker'
Vagrant.require_version ">= 1.6.0"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
    config.vm.define "ocb128_crypt" do |erlang|
        erlang.vm.provider "docker" do |d|
            d.image = "freke/jenkins-slave-erlang"
            d.name = "ocb128_crypt"
            d.has_ssh = true
        end
    end
end
