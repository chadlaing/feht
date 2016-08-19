# **_feht_** -- pronounced "fate", as the "eh" is Canadian

A commandline program to automatically identify markers predictive of groups. Can be used with binary data, genomic (single nucleotide variant) data, or arbitrary character data.

## Quick Start


## Descriptiion

The program takes command line arguments, 3 of which are required. For example:

    ./feht --info=data/metadata.txt --datafile=data/data.tab --mode="snp.txt" > output.txt
 
Files can be either tab-delimited, or comma-delimited. The default is tab, but it can be changed by including
 
    --delimter=","
             
in the command line options. Both the metadata file and the datafile must use the same delimiter.
 
The info file is the delimited metadata, where the column headers denote the metadata categories, the row labels denote the subject names (which must exactly match the names in the datafile), and the cells are values for the metadata category for a given subject.
 
The datafile is the actual data, where the column headers denote the subject names (which must exactly match the row labels in the info file), the row labels denote the factor name (which must be unique), and the cells are the factors for a particular subject. If the factors represent binary data, this can be specified as:

    --mode="binary"
 
If the mode is set as “snp”, using:
 
    --mode="snp
 
 then the factors are assumed to be genetic data of A, C, T, or G, and each of A vs not-A, C vs not-C, T vs not T, G vs not-G are computed, and (_c, _t, _g, _a) is appended to the factor name in the results, to denote the comparison that was significant.
 
There are two additional, optional arguments: `--one`, and `--two`, which are used to specify the Metadata Category and values for groups of interest. If neither `--one` nor `--two` are specified, then all possible combinations for all metadata categories will be computed eg. if there is a Province column, then AB vs. not-AB, AB vs. BC, AB vs. SK, AB vs. MB ... will be computed, and this is done for every column in the `--info` file.
 
If only `--one` is specified, then that group is compared against all others of the same category. Using our previous Province example, to compare AB and NB to a group consisting of all other provinces, the following would be run:

    ./feht --info=data/metadata.txt --datafile=data/data.tab --mode="snp" --one="Province AB NB" > output.txt
 
The options given to --one must be quoted, and the first word must contain no spaces and be an exact match to a Column Name in the `--info` sheet, followed by space-separated values that should be included as part of the comparison group.
 
The same applies to specifying arguments to `--two`, and if specified in addition to `--one`, only those two groups will be compared. For example:

    ./feht --info=data/metadata.txt --datafile=data/data.tab --mode="snp" --one="Province AB NB" --two="SourceState NS QC OE" > output.txt
