.stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz: .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot
	rm -f .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz
	xz -9 --keep .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot

upload: .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz
	scp .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz azureuser@mineserverchile.cloudapp.net:./
