#ifndef _SEISWPLOT_H_
#define _SEISWPLOT_H_
/* This is cloned from dbxcor.  It is used as a compact data structure to sent to 
   a procedure immediately below (BuildMenu) that constructs a widget based on this specification*/
typedef struct _menu_item
{
        char              *label;          /* the label for the item */
        WidgetClass       *class1;          /* pushbutton, label, separator... */
        char               mnemonic;       /* mnemonic; NULL if none */
        char              *accelerator;    /* accelerator; NULL if none */
        char              *accel_text;     /* to be converted to compound string */
        void             (*callback)(Widget,void *,void *);    /* routine to call; NULL if none */
        XtPointer          callback_data;  /* client_data for callback() */
        Widget           w;
        struct _menu_item *subitems;       /* pullright menu items, if not NULL */
} MenuItem;
Widget BuildMenu (Widget parent, int menu_type, 
        char *menu_title, char menu_mnemonic,
                 Boolean tear_off, MenuItem *items);
#endif
