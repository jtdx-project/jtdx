void export_wisdom_(char fname[], int len)
{
  int fftwf_export_wisdom_to_filename(const char *);
  fname[len-1]=0;
  fftwf_export_wisdom_to_filename(fname);
}

void import_wisdom_(char fname[], int *success, int len)
{
  int fftwf_import_wisdom_from_filename(const char *);
  fname[len-1]=0;
  *success = fftwf_import_wisdom_from_filename(fname);
}
