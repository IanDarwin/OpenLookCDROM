/*
 * Version control hack.
 *
 * The purpose of this file is to define a variable named
 * link_with_libfxrpc so that we ensure people link with
 * THIS version of the rpc library.
 *
 * On an RS/6000, or any other platform that has a librpc
 * which is compatible with this one, we create libfxrpc.a
 * with this file ONLY.
 */

int link_with_libfxrpc;
