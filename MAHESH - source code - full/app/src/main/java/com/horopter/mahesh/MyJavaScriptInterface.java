package com.horopter.mahesh;

import android.content.Context;
import android.webkit.JavascriptInterface;

/**
 * Created by Horopter on 5/13/2016.
 */
class MyJavaScriptInterface {

    private Context ctx;

    MyJavaScriptInterface(Context ctx) {
        this.ctx = ctx;
    }

    @JavascriptInterface
    public void showHTML(String html) {
        System.out.println(html);
    }

}
