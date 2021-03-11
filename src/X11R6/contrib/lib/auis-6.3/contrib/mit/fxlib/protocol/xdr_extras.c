/*
 * XDR routine for a KTEXT structure.
 */

bool_t xdr_KTEXT_ST(xdrs, auth)
    XDR *xdrs;
    KTEXT auth;
{
    return xdr_opaque(xdrs, auth, sizeof(KTEXT_ST));
}
