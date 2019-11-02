#include <windows.h>

// from https://github.com/ghc/ghc/blob/4b431f334018eaef2cf36de3316025c68c922915/utils/fs/fs.c
HANDLE *fdOpen(LPCWSTR filename) {
    // Construct access mode.
    // https://docs.microsoft.com/en-us/windows/win32/fileio/file-access-rights-constants
    DWORD dwDesiredAccess = GENERIC_WRITE | GENERIC_READ;

    // Construct shared mode.
    // https://docs.microsoft.com/en-us/windows/win32/fileio/file-attribute-constants
    DWORD dwShareMode = FILE_SHARE_READ | FILE_SHARE_WRITE;

    // Create file disposition.
    // https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilew
    DWORD dwCreationDisposition = OPEN_ALWAYS;

    // Set file access attributes.
    DWORD dwFlagsAndAttributes = FILE_ATTRIBUTE_NORMAL;

    SECURITY_ATTRIBUTES securityAttributes;
    ZeroMemory (&securityAttributes, sizeof(SECURITY_ATTRIBUTES));
    securityAttributes.bInheritHandle       = TRUE; // not sure what should be here
    securityAttributes.lpSecurityDescriptor = NULL;
    securityAttributes.nLength              = sizeof(SECURITY_ATTRIBUTES);

    HANDLE res = CreateFileW(filename, dwDesiredAccess, dwShareMode, &securityAttributes,
        dwCreationDisposition, dwFlagsAndAttributes, NULL);

    return res;
}
