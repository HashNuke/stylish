compile:
	cd priv/sassc/ && SASS_LIBSASS_PATH=../libsass make

clean:
	cd priv/sassc/ && SASS_LIBSASS_PATH=../libsass make clean
