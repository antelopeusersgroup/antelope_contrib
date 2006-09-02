#ifndef __XCOR_GUI_H
#define __XCOR_GUI_H

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

#endif
