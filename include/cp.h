#ifndef __CP_H_
#define __CP_H_

BOOL    cp_CD               (char* path);
BOOL	cp_DIR              (char* path);
BOOL    cp_MKDIR            (char* filename);
BOOL    cp_DEL              (char* filename);
BOOL    cp_exec_batch_file  (char* filename);
BOOL    cp_type             (char* filename);

BOOL    cp_process ();
void    cp_run ();

#endif