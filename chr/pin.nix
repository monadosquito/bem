{
    miso
        =
        import
            (fetchTarball https://github.com/dmjio/miso/archive/1.7.1.tar.gz);
    traverse
        =
        import
            (fetchTarball
                 https://github.com/monadosquito/traverse/archive/v1.0.0.tar.gz
            );
    unpath
        =
        import
            (fetchTarball
                 https://github.com/monadosquito/unpath/archive/v1.0.0.tar.gz
            );
}
