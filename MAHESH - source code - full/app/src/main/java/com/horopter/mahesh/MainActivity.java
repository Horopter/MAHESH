package com.horopter.mahesh;

import android.support.v7.app.AppCompatActivity;
import android.os.Bundle;
import android.view.KeyEvent;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;

public class MainActivity extends AppCompatActivity {
    private WebView view;
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        view = new WebView(this);
        view.getSettings().setJavaScriptEnabled(true);
        view.loadUrl("http://moviessmvit.github.io/MoviesInfo/index.html");
        view.setWebViewClient(new WebViewClient(){
            @Override
            public boolean shouldOverrideUrlLoading(WebView view,String url)
            {
                view.loadUrl(url);
                return false;
            }
        });
        view.setWebChromeClient(new WebChromeClient());
        MyJavaScriptInterface javaScriptInterFace=new MyJavaScriptInterface(this);
        view.addJavascriptInterface(javaScriptInterFace, "AndroidFunction");
        setContentView(view);
    }
    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN) {
            switch (keyCode) {
                case KeyEvent.KEYCODE_BACK:
                    if (view.canGoBack()) {
                        view.goBack();
                    } else {
                        finish();
                    }
                    return true;
            }

        }
        return super.onKeyDown(keyCode, event);
    }
}
