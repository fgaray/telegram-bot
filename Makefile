.stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz: .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot
	rm -f .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz
	strip .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot
	xz -9 --keep .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot

upload: .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz
	scp -r .stack-work/install/x86_64-linux-nopie/lts-8.3/8.0.2/bin/los-programadores-bot.xz markov* azureuser@mineserverchile.cloudapp.net:./
