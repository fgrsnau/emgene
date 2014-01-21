emgene
======

How to build
------------

    cabal sandbox init
    cabal install --only-dependencies
    cabal build

How to run
----------

    cabal run emgene -- --help

Example input
-------------

    K=11 N=20 L=111
    atcattttctttattatctttttgtcctttttgtatgtgcggtgtaaaatTTCTTATCTGTaaatggcgcaggctcttcctctttttctactggtcttttggaagaaaaag
    gcaaaacaatcttgacaccatgtaggcgatttatgcctctaagtcttggtTTCTTATCTGTgaaatgggggaggaaaatgcttgcagtcggagcctgaccacaaactgact
    gttgatgatgatgatgttgtctagagaaattttaacttttaaaaagcactTCCTTATCTTTtacatttttctgctgtgaactaggtatttcttatctgtgtgttcctcccc
    ttaaaaagcacttccttatcttttacatttttctgctgtgaactaggtatTTCTTATCTGTgtgttcctccccatccctggcttctggtgttttacccatatctaagatag
    catcagcaagcagttcctctccctcattctgcaaatgtgtgagttctgctTTCTTATCTGTctcttatcacttcacagcagtgtgaccctgaaccagttccctgtaaaaga
    agttcctctccctcattctgcaaatgtgtgagttctgctttcttatctgtCTCTTATCACTtcacagcagtgtgaccctgaaccagttccctgtaaaagactagggaggat
    gctgctcagaccaagaatggggaagtggggctgggagacaaagaaattccTTCTTATCTGTgtaggatacacatgtctgagaggcagagatcagatgtcctgccctctgat
    aaacggggctacaagagcaagactcatacacactttgattgcacaactccTTCTTATCTGTtcagcacagctgacctgccgagggagtcaccatagtggttggctgtggcc
    gtaaactacaaacaaaacaaaatggccatccggcttctttcttccttcggTTCTTATCTGTgacctaagctggcttgcctggaatttactaaatagactaggtcagccgtg
    ggacagggagagaccagagaacaccacactaaatgaattctgccagacatTTCTTATCTGTggctcacactctttatctatccataaagtcatgcacaaacaaaggacaaa
    acaccacactaaatgaattctgccagacatttcttatctgtggctcacacTCTTTATCTATccataaagtcatgcacaaacaaaggacaaaaatggagaagtgaaactgtg
    gtccacaaaccttgagctttcacgctgtgtataaactctgcattggacttTTCTTATCTGTcatgcccagggcaaggtaggactcttatcaactatgaacactagcatgac
    gtgtagctcaacataaggacacagcacaggaagttggctcatgtttgataTTCTTATCTGTtgaaagttatgttgggaggacagtactattaagataaccacaagagcaag
    tcttggaaattatctttcctgtcacagaatagagcatcataataacgtgtTTCTTATCTGTcctgtccacctttcagcaagaaacctcagaaagtatgttttctattgcta
    aggctacagaacttgcatgtcactggcggtggtaaataatcatctcccgtTTCTTATCTGTctgtgggtttcctctgtggttcagtgacggggcttgtcagtcaggtgaga
    actcatgcccagcaggcacgctgctgtctgcgtcttttcctcgataacagTTCTTATCTGTgttgtgcaatagctgtgatataattgtagcagaatgggaggttgcataac
    ctccccagcagcttgccttaaactaggtaagaactttccattcttaagcaTTCTTATCTGTggaggcttttaggggtacaccctaacaacatctcttctggacacacctaa
    atccgttgagtggaagcatgctctggtaaattgcattacctcagcccccaTTCTTATCTGTgtgtcacacttgtcttataggaaataagcagtgtacagagaaatgtgccc
    aataacaccttactgtggtcagtatttattgtctacatgagcctaagaccTTCTTATCTGTggtcccacagtgtgaaatcctttgaaacttgcattgcccaagagagtgct
    aggcttccatggcagtccatcgctgctgagggaggagttactctgttctcTTCTTATCTGTaatacacaaacatgagttaaaatgagagctaagctgcaaaggacaacaaa

Example output
--------------

Run program with `./emgene -f INPUTFILE`:

    Found best motif: [T,T,C,T,T,A,T,C,T,G,T]
    M_rel = 4x11
    -4.392  -4.392  -4.392  -4.392  -4.392   1.877  -4.392  -4.392  -4.392  -4.392  -4.392
    -4.392  -4.390   2.102  -4.392  -4.392  -4.392  -4.392   2.102  -4.392  -4.392  -4.392
    -4.392  -4.392  -4.392  -4.392  -4.392  -4.392  -4.392  -4.392  -4.392   2.256  -4.392
     1.634   1.634  -4.392   1.634   1.634  -4.392   1.634  -4.392   1.634  -4.390   1.634
    Found indices: [51,51,89,51,51,40,51,51,51,51,31,51,51,51,51,51,51,51,51,51]
    N=20 K=11 L=111
    atcattttctttattatctttttgtcctttttgtatgtgcggtgtaaaatTTCTTATCTGTaaatggcgcaggctcttcctctttttctactggtcttttggaagaaaaag
    gcaaaacaatcttgacaccatgtaggcgatttatgcctctaagtcttggtTTCTTATCTGTgaaatgggggaggaaaatgcttgcagtcggagcctgaccacaaactgact
    gttgatgatgatgatgttgtctagagaaattttaacttttaaaaagcacttccttatcttttacatttttctgctgtgaactaggtatTTCTTATCTGTgtgttcctcccc
    ttaaaaagcacttccttatcttttacatttttctgctgtgaactaggtatTTCTTATCTGTgtgttcctccccatccctggcttctggtgttttacccatatctaagatag
    catcagcaagcagttcctctccctcattctgcaaatgtgtgagttctgctTTCTTATCTGTctcttatcacttcacagcagtgtgaccctgaaccagttccctgtaaaaga
    agttcctctccctcattctgcaaatgtgtgagttctgctTTCTTATCTGTctcttatcacttcacagcagtgtgaccctgaaccagttccctgtaaaagactagggaggat
    gctgctcagaccaagaatggggaagtggggctgggagacaaagaaattccTTCTTATCTGTgtaggatacacatgtctgagaggcagagatcagatgtcctgccctctgat
    aaacggggctacaagagcaagactcatacacactttgattgcacaactccTTCTTATCTGTtcagcacagctgacctgccgagggagtcaccatagtggttggctgtggcc
    gtaaactacaaacaaaacaaaatggccatccggcttctttcttccttcggTTCTTATCTGTgacctaagctggcttgcctggaatttactaaatagactaggtcagccgtg
    ggacagggagagaccagagaacaccacactaaatgaattctgccagacatTTCTTATCTGTggctcacactctttatctatccataaagtcatgcacaaacaaaggacaaa
    acaccacactaaatgaattctgccagacatTTCTTATCTGTggctcacactctttatctatccataaagtcatgcacaaacaaaggacaaaaatggagaagtgaaactgtg
    gtccacaaaccttgagctttcacgctgtgtataaactctgcattggacttTTCTTATCTGTcatgcccagggcaaggtaggactcttatcaactatgaacactagcatgac
    gtgtagctcaacataaggacacagcacaggaagttggctcatgtttgataTTCTTATCTGTtgaaagttatgttgggaggacagtactattaagataaccacaagagcaag
    tcttggaaattatctttcctgtcacagaatagagcatcataataacgtgtTTCTTATCTGTcctgtccacctttcagcaagaaacctcagaaagtatgttttctattgcta
    aggctacagaacttgcatgtcactggcggtggtaaataatcatctcccgtTTCTTATCTGTctgtgggtttcctctgtggttcagtgacggggcttgtcagtcaggtgaga
    actcatgcccagcaggcacgctgctgtctgcgtcttttcctcgataacagTTCTTATCTGTgttgtgcaatagctgtgatataattgtagcagaatgggaggttgcataac
    ctccccagcagcttgccttaaactaggtaagaactttccattcttaagcaTTCTTATCTGTggaggcttttaggggtacaccctaacaacatctcttctggacacacctaa
    atccgttgagtggaagcatgctctggtaaattgcattacctcagcccccaTTCTTATCTGTgtgtcacacttgtcttataggaaataagcagtgtacagagaaatgtgccc
    aataacaccttactgtggtcagtatttattgtctacatgagcctaagaccTTCTTATCTGTggtcccacagtgtgaaatcctttgaaacttgcattgcccaagagagtgct
    aggcttccatggcagtccatcgctgctgagggaggagttactctgttctcTTCTTATCTGTaatacacaaacatgagttaaaatgagagctaagctgcaaaggacaacaaa

<!-- vim: set ts=4 sts=4 sw=4 et ft=markdown: -->
