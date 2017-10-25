# **_feht_**
## pronounced "fate", as the "eh" is Canadian
[![Master branch build status](https://travis-ci.org/chadlaing/feht.svg?branch=master "Master Build Status")](https://travis-ci.org/chadlaing/feht)

A commandline program to automatically identify markers predictive of groups. Can be used with binary data, genomic (single nucleotide variant) data, or arbitrary character data.

If you are on Windows and prefer a GUI, check out [GenomeFisher](https://bitbucket.org/peterk87/genomefisher/wiki/Home)

## Descriptiion

### All commandline options 

    feht - predictive marker discovery

    Usage: feht (-i|--infoFile FILE) (-d|--datafile FILE)
                [--one "Group1Name Group1Item Group1Item Group1Item"]
                [--two "Group2Name Group2Item Group2Item Group2Item"]
                [-l|--delimiter [',', '\t' ...], DEFAULT='\t']
                [-m|--mode ['binary', 'snp'], DEFAULT='binary']
                [-c|--correction ['none', 'bonferroni'], DEFAULT='bonferroni']
                [-f|--ratioFilter [Filter results by ratio (0.00-1.0), DEFAULT=0]]
    Predictive marker discovery for groups; binary data, genomic data (single nucleotide variants), and arbitrary character data.

    Available options:
    -i,--infoFile FILE       File of metadata information
    -d,--datafile FILE       File of binary or single-nucleotide variant data
    --one "Group1Name Group1Item Group1Item Group1Item"
                           Group1 column name, followed by optional Group1
                           labels to include as part of the group
    --two "Group2Name Group2Item Group2Item Group2Item"
                           Group2 column name, followed by optional Group2
                           labels to include as part of the group
    -l,--delimiter [',', '\t' ...], DEFAULT='\t'
                           Delimiter used for both the metadata and data file
    -m,--mode ['binary', 'snp'], DEFAULT='binary'
                           Mode for program data; either 'binary' or 'snp'
    -c,--correction ['none', 'bonferroni'], DEFAULT='bonferroni'
                           Multiple-testing correction to apply
    -f,--ratioFilter [Filter results by ratio (0.00-1.0), DEFAULT=0]
                           Display only those results greater than or equal to
                           the value
    -h,--help              Show this help text


### Installation
`feht` is written in Haskell and is most easily built using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/).

Taken from the Stack documentation above, Stack can be installed on a Unix-like system by running:

    wget -qO- https://get.haskellstack.org/ | sh
    
Following the installation of Stack, `feht` can be built and tested by running in the project root directory:

    stack test 
    
All tests should pass, following which:

    stack install 
    
Will copy the compiled executable to your local `bin/` path.

Compiled binaries of `feht` for Linux and Windows can be found here: [binaries](https://github.com/chadlaing/feht/releases)



### File format 

The program takes command line arguments, of which 2 are required: `-i`, which specifies  the information (eg. metadata) file, and `-d`, which specifies the data file. Both of these files need to be delimited with the same delimiter, eg. tab (`\t`, which is the default).

The information file should be formatted with sample names in the first column, which does not require a header; sample names need to be identical in both the information and data files. All other columns require a header, and this header will be used as a metadata category; all subsequent rows will be interpreted as values within that category. For example, the `data/test_metadata.txt` file included in this repository is as follows:

    genomes	group	position
    GenomeA	B	up
    GenomeB	A	up
    GenomeC	A	down
    GenomeD	C	sideways
    GenomeE	B	down
    GenomeF	A	down
    GenomeG	A	floating
    GenomeH	A	up
    GenomeI	B	sideways
    GenomeJ	C	down
    GenomeK	B	up

The first column contains the sample names, `GenomeA, GenomeB ...` and though not required also contains a column header. Both `group` and `postion` will be interpreted as metadata categories, and `A, B, C` as values within metadata category `group`, and `up, down, sideways, floating` as values with metadata category `position`.

The corresponding data file `data/test_binary.txt` looks as follows:
	
        GenomeA	GenomeB	GenomeC	GenomeD	GenomeE	GenomeF	GenomeG	GenomeH	GenomeI	GenomeJ	GenomeK	GenomeL
    binary1	0	-	0	0	1	0	0	1	0	0	1	1
    binary2	0	0	0	0	0	0	0	0	0	0	0	0
    binary3	1	1	0	0	0	0	1	1	1	1	0	0
    binary4	1	0	0	1	1	0	0	-	0	0	1	0
    ... (truncated for space)

In the data file, the sample names are the column headers, and must exactly match those provided in the information (metadata) file. The first column in the data file lacks a column header, but contains labels for the data being examined, in this case `snp1, snp2, snp3, snp4 ...`. Each row represents values of the data being examined for each sample.

### Performing comparisons

#### All possible pairwise comparisons
`feht` by default will perform all possible pairwise comparisons given the categories in the information file. In our example using the `data/test_metadata.txt` file, a separate comparison within the `group` category of (`A vs. B`, `A vs. C`, `A vs (B and C)`,`B vs. C`, `B vs. (A and C)`, `C vs. (A and B)`) will be performed, and likewise within the `position` category. With our test data, these comparisons can be run with:

    feht -i data/test_metadata.txt -d data/test_binary.txt
    
If you wish to save the results to a file, pipe them to the filename of your choice:

    feht -i data/test_metadata.txt -d data/test_binary.txt > results.txt
    
And will produce an output file sorted from "most" to "least" discriminatory data. In our example that looks like: 

    [#-
    Group1 category: group Group1: B
    Group2 category: group Group2: C
    ---
    Name	GroupOne (+)	GroupOne (-)	GroupTwo (+)	GroupTwo (-)	pValue	Ratio
    binary21	4	0	0	2	1.0	1.0
    binary44	3	1	0	2	1.0	0.75
    binary42	3	1	0	2	1.0	0.75
    binary24	1	3	1	0	1.0	-0.75
    ...
    -#]

    [#-
    Group1 category: position Group1: sideways
    Group2 category: position Group2: up
    ---
    Name	GroupOne (+)	GroupOne (-)	GroupTwo (+)	GroupTwo (-)	pValue	Ratio
    binary9	2	0	1	4	1.0	0.8
    binary47	2	0	1	4	1.0	0.8
    binary8	0	2	4	1	1.0	-0.8
    binary49	2	0	1	4	1.0	0.8
    binary1	0	2	3	1	1.0	-0.75
    ...
    -#]
   
    ...

Each output block lists the categories that are being compared, and the values within the category that constitute the group. For example, the first output block above is a comparison between `B` and `C` within the `group` category. The output consists of seven columns, the first being the data label that was compared, and the next four showing the presence and absence of that particular datum among the two groups. In the first example above, for the datum `binary21`, `GroupOne` (which is `B` from the category `group`) contained four members that were positive for `binary21` and 0 that were negative. For `GroupTwo` (which is `C` from the category `group`) there were no members that were positive for `binary21` and two members that were negative. 

The next column is the P-value, which shows by default the `bonferroni` corrected value. In this example, due to the small sample size and number of comparisons, the corrected value is not significant (eg. `1.0`). 

The final column contains the ratio of the fraction of `GroupOne` positive minus the fraction of `GroupTwo` positive. In our example for `binary21` this is (4/4 - 0/2), which gives the result of `1.0`. The ratio provides an additional method for identifying data that are skewed between the groups under comparison. A value of `1.0` means that all of `GroupOne` was positive for the datum and all of `GroupTwo` was negative; conversely a ratio of `-1.0` means that all of `GroupOne` was negative, and all of `GroupTwo` was positive.

#### Specifying groups
By default all pairwise comparisons will be computed, but user-specified groups can be given as well. In our example, if we only wanted to compare `A` and `B` in the `group` category, we could specify both `GroupOne` and `GroupTwo` as follows:

    
    feht -i data/test_metadata.txt -d data/test_binary.txt --one "group A" --two "group B"
    
More than one value per category can be specified, as follows:


    feht -i data/test_metadata.txt -d data/test_binary.txt --one "position up down" --two "position sideways floating"
    
Easily performing a one vs. all comparison is done by specifying only `GroupOne`, which will then be compared to a group comprised of all non-specified values of the same category. For example:


    feht -i data/test_metadata.txt -d data/test_binary.txt --one "position up"
    
The above will construct `GroupOne` as `up` and `GroupTwo` as `down sideways floating`.

### Filtering the results
By default, `feht` will output every result for every comparison. If you wish to limit the number of results, the `ratioFilter` can be used, where only results with a ratio greater than or equal to the value will be displayed. For example, in the first block of results above, if we set the `ratioFilter` to `1.0` as in the following:

    feht -i data/test_metadata.txt -d data/test_binary.txt -f 1

Only a single result is returned:

    [#-
    Group1 category: group Group1: B
    Group2 category: group Group2: C
    ---
    Name	GroupOne (+)	GroupOne (-)	GroupTwo (+)	GroupTwo (-)	pValue	Ratio
    binary21	4	0	0	2	1.0	1.0
    -#]

### Specifying a delimiter

By default the tab character ('\t') is used as a delimiter, but any single character can be used. To use the comma character (',') enter it using the `-l` argument ("el"), and single-quotes around the delimiter:


    feht -i data/test_metadata.txt -d data/test_binary.txt -l ','

### Turning off multiple-testing correction

If desired, the multiple-testing correction can be turned off by specifying "none" to the `-c` option. For example, to run a comparison with no correction:

    feht -i data/test_metadata.txt -d data/test_binary.txt -c none
    
### Built-in data types 
`feht` by default operated on a table of binary data, but comes with built-in support for single-nucleotide variant (SNV) data.

For each data entry, `feht` will convert the SNV into a binary comparison for all four nucleotides. Consider the provided data in `data/test_snps.txt`:

        GenomeA	GenomeB	GenomeC	GenomeD	GenomeE	GenomeF	GenomeG	GenomeH	GenomeI	GenomeJ	GenomeK	GenomeL
    snp1	T	-	T	T	C	A	A	C	A	A	C	C
    snp2	A	T	T	A	A	A	T	A	A	T	A	T
    snp3	C	G	T	T	A	A	G	C	C	C	A	A
    snp4	C	T	A	C	G	T	T	-	A	A	C	A
    ...
    
If the following comparison is run:


    feht -i data/test_metadata.txt -d data/test_snps.txt -m snp 
    
The following is produced:

    [#-
    Group1 category: group Group1: B
    Group2 category: group Group2: C
    ---
    Name	GroupOne (+)	GroupOne (-)	GroupTwo (+)	GroupTwo (-)	pValue	Ratio
    snp32_a	0	4	2	0	1.0	-1.0
    snp45_a	0	4	2	0	1.0	-1.0
    snp41_t	0	4	2	0	1.0	-1.0
    snp21_t	0	4	2	0	1.0	-1.0   
    ...
    
This shows that the data for `snp32` has been converted into binary, with the nucleotide under comparison appended. For `snp32_a` this represents all `A` characters in the data as positive, and all `C`, `T`, and `G` characters as negative. All four comparisons are carried out for each row of SNV data.

### Missing data
Within the `binary` mode, if data are not in `1` or `0` form, they will be ignored, and will not contribute to the calculations as either a positive or negative value; the total data for groups will be adjusted to accommodate the missing data. The same is true for `snp` mode data that is not `A, C, T, G` For example:


        GenomeA	GenomeB	GenomeC	GenomeD	GenomeE	GenomeF	GenomeG	GenomeH	GenomeI	GenomeJ	GenomeK	GenomeL
    snp1	T	-	T	T	C	A	A	C	A	A	C	C

Contains 12 possible entries, but the one for `GenomeB` is of the form `-`. This entry will be ignored, and only 11 data points will be used. This means that if `GroupOne` normally has 4 members and contains `GenomeB`, for the `snp1` calculations, it will be as if it only contained 3 members. 

