#include <map>
#include <tuple>
#include <windows.h>
#include <memory>
#include <coroutine>
#include <cassert>
#include <array>
#include <bit>
#include <thread>
#include <vector>
#include <mmsystem.h>
#include <cmath>

#pragma comment(lib, "winmm.lib")

class BeepControl {
    static constexpr int sampleRate = 44000;       // 44kHz
    static constexpr int durationMs = 100;       // 0.2秒
    static constexpr int samples = sampleRate * durationMs / 1000;
    static constexpr double frequency = 880.0;    // A5
    std::vector<BYTE> buffer_;
    HWAVEOUT hWaveOut_;
    WAVEHDR hdr_ = {};
public:
    void Play() {
        if (hdr_.dwFlags & (WHDR_PREPARED|WHDR_DONE)) {
            waveOutWrite(hWaveOut_, &hdr_, sizeof(hdr_));
        }
    }
    BeepControl()
        : buffer_(samples) {
        for (int i = 0; i < samples; ++i) {
            double t = static_cast<double>(i) / sampleRate;
            buffer_[i] = static_cast<BYTE>(127.5 * (sin(2 * 3.141592 * frequency * t)*0.1 + 1));
        }
        WAVEFORMATEX wfx = {};
        wfx.wFormatTag = WAVE_FORMAT_PCM;
        wfx.nChannels = 1;
        wfx.nSamplesPerSec = sampleRate;
        wfx.wBitsPerSample = 8;
        wfx.nBlockAlign = wfx.nChannels * wfx.wBitsPerSample / 8;
        wfx.nAvgBytesPerSec = wfx.nSamplesPerSec * wfx.nBlockAlign;
        waveOutOpen(&hWaveOut_, WAVE_MAPPER, &wfx, 0, 0, CALLBACK_NULL);
        hdr_.lpData = reinterpret_cast<LPSTR>(buffer_.data());
        hdr_.dwBufferLength = samples;
        waveOutPrepareHeader(hWaveOut_, &hdr_, sizeof(hdr_));
    }
    ~BeepControl() {
        waveOutUnprepareHeader(hWaveOut_, &hdr_, sizeof(hdr_));
        waveOutClose(hWaveOut_);
    }
};

class Window; // Forward declaration
static std::map<HWND, std::tuple<LPCWSTR,Window*>> g_map_debug_;

class HInstance {
    HINSTANCE hInstance_;
public:
    HINSTANCE Handle() const { return hInstance_; }
    HInstance(HINSTANCE hInstance) :
        hInstance_(hInstance) {}
};

std::unique_ptr<HInstance> g_hInstance;

#define TIMER_FRAME 1

RECT screenRect;
POINT velocity = { 1, 1 };
constexpr SIZE size_pixel = { 10,10 };
constexpr SIZE size_frame = { 49, 70 };
constexpr SIZE size_paddle = { 7* size_pixel.cx,1 };
constexpr SIZE size_block = { 5,3 };
constexpr SIZE num_block = { 10, 5 };
constexpr POINT pos_blocks = { 0,8 };
POINT pos_ball = { 0,0 };

class Window {
    HWND handle_;
    LPCWSTR className_ = nullptr;
    std::atomic_bool should_close_ = false;
    static ::LRESULT CALLBACK DummyWndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
        switch (msg) {
        case WM_SETFOCUS:
            // 親ウィンドウにフォーカスを戻す
            HWND hwndParent = GetParent(hwnd);
            if (hwndParent) {
                SetFocus(hwndParent);
            }
        }
        return DefWindowProc(hwnd, msg, wParam, lParam);
    }
    void Initialized() {
        //g_map_debug_.emplace(handle_, std::make_tuple(className_, this));
        SetWindowLongPtr(
            handle_,
            GWLP_USERDATA,
            reinterpret_cast<LONG_PTR>(this)
        );
    }
public:
    void TopMost() const {
        SetWindowPos(handle_, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
    }
    void SetWindowProc(WNDPROC wndProc) {
        Initialized();
        SetWindowLongPtr(handle_, GWLP_WNDPROC, reinterpret_cast<LONG_PTR>(wndProc));
	}
    void ShouldClose(bool should_close) {
        should_close_ = should_close;
	}
	bool ShouldClose() const { return should_close_; }
    HWND Handle() const { return handle_; }
    BOOL Show(int cmd) {
        return ShowWindow(handle_, cmd);
    }
    UINT_PTR SetTimer(UINT_PTR nIDEvent, UINT uElapse, TIMERPROC lpTimerFunc) {
        return ::SetTimer(handle_, nIDEvent, uElapse, lpTimerFunc);
    }
    BOOL SetPos(
        const Window* window_insert_after,
        int x, int y,
        int cx, int cy,
        UINT flags) {
        return ::SetWindowPos(
            handle_,
            window_insert_after ? window_insert_after->Handle() : NULL,
            x, y, cx, cy,
            flags
        );
    }
    BOOL InvalidateRect(const RECT* lpRect, BOOL bErase) {
        return ::InvalidateRect(handle_, lpRect, bErase);
    }
    BOOL Update() {
        return ::UpdateWindow(handle_);
    }
    BOOL MoveWindow(
        int X,
        int Y,
        int nWidth,
        int nHeight,
        BOOL bRepaint = TRUE
    ) {
		return ::MoveWindow(handle_, X, Y, nWidth, nHeight, bRepaint);
    }
    BOOL MoveWindowBlox(
        int X,
        int Y,
        int nWidth,
        int nHeight,
        BOOL bRepaint = TRUE
    ) {
        return ::MoveWindow(
            handle_,
            X * size_pixel.cx, Y * size_pixel.cy,
            nWidth * size_pixel.cx, nHeight * size_pixel.cy,
            bRepaint
        );
    }
    Window(const Window&) = delete;
    Window(
        LPCWSTR className,
        int brush,
        DWORD ws,
        SIZE size,
        Window* window_parent
    ) : handle_([](
            LPCWSTR className,
            int brush,
            DWORD ws,
            SIZE size,
            Window* window_parent
        )->HWND {
        WNDCLASS wc = { 0 };
        wc.lpfnWndProc = DummyWndProc;
        wc.hInstance = g_hInstance->Handle();
        wc.lpszClassName = className;
        wc.hbrBackground = (HBRUSH)GetStockObject(brush);
        wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);

        RegisterClass(&wc);

        auto handle = CreateWindowEx(
            0,
            wc.lpszClassName,
            L"",
            ws | WS_VISIBLE,
            0, 0, size.cx * size_pixel.cx, size.cy * size_pixel.cy,
            Handle(window_parent),
            NULL,
            wc.hInstance,
            NULL
        );
        return handle;
    }(className,brush,ws,size,window_parent)),
    className_(className) {
    }
    ~Window() {
        DestroyWindow(handle_);
    }
    static Window* FromHwnd(HWND hwnd) {
        //auto it = g_map_debug_.find(hwnd);
        //if (it == g_map_debug_.end()) {
        //    return nullptr;
        //}
        //return std::get<1>(it->second);
        return reinterpret_cast<Window*>(GetWindowLongPtr(hwnd, GWLP_USERDATA));
    }
    static HWND Handle(Window* window) {
        if (window == nullptr) return NULL;
        return window->handle_;
    }
    static void MessageLoop() {
        MSG msg;
        while (GetMessage(&msg, NULL, 0, 0)) {
            TranslateMessage(&msg);
            DispatchMessage(&msg);
        }
    }
};

static HRGN RgnDot(int x, int y) {
    x *= size_pixel.cx, y *= size_pixel.cy;
    auto rgn = CreateRectRgn(
        x, y,
        x + size_pixel.cx, y + size_pixel.cy
    );
	return rgn;
}
static HRGN RgnBlock(int x, int y) {
    constexpr int width = size_block.cx;
	constexpr int height = size_block.cy;
    x *= size_pixel.cx*width, y *= size_pixel.cy*height;
    auto rgn = CreateRectRgn(
        x, y,
        x + (width-1)*size_pixel.cx, y + (height-1)*size_pixel.cy
    );
    return rgn;
}
static HRGN RgnOr(HRGN rgn1, HRGN rgn2) {
    auto rgn = CreateRectRgn(0, 0, 0, 0);
    auto result = CombineRgn(rgn, rgn1, rgn2, RGN_OR);
    assert(result != ERROR);
	DeleteObject(rgn1);
	DeleteObject(rgn2);
    return rgn;
}

class WindowBlocks : public Window {
	std::array<uint16_t, num_block.cy> blocks_;
    void ApplyBlocks() {
        auto rgn = CreateRectRgn(0, 0, 0, 0);
        for(int y=0; y<blocks_.size(); y++) {
            auto bits = blocks_[y];
            for (int x = 0; x < 16; x++, bits>>=1U) {
                if (!(bits & 1)) continue;
                auto rgn_add = RgnBlock(x, y);
				rgn = RgnOr(rgn, rgn_add);
			}
        }
        SetWindowRgn(Handle(), rgn, TRUE);
        DeleteObject(rgn);
    }
public:
    int GetRemain() const {
        int count = 0;
        for (auto bits : blocks_) {
			count += std::popcount(bits);
        }
        return count;
    }

    bool Collision(POINT pos, POINT& velocity) {
        pos.x += velocity.x;
        pos.y += velocity.y;
		pos.x /= size_pixel.cx;
		pos.y /= size_pixel.cy;
        auto x_block = pos.x / size_block.cx;
        auto y_block = pos.y / size_block.cy;
        if (y_block < 0) return false;
        if (blocks_.size() <= y_block) return false;
        auto exist = blocks_[y_block] & (1U << (x_block));
        if (!exist) return false;
        auto hit_x = false;
        if (velocity.x < 0) {
            hit_x = ((pos.x % size_block.cx) == size_block.cx - 2);
        }
        else {
            hit_x = ((pos.x % size_block.cx) == 0);
        }
        auto hit_y = false;
        if (velocity.y < 0) {
            hit_y = ((pos.y % size_block.cy) == size_block.cy - 2);
        }
        else {
            hit_y = ((pos.y % size_block.cy) == 0);
        }
        if (hit_x && hit_y) {
            if (abs(velocity.x) < abs(velocity.y)) {
                hit_x = false;
            }
            else {
                hit_y = false;
            }
        }
        if (hit_x) {
            velocity.x = -velocity.x;
        }
        if (hit_y) {
            velocity.y = -velocity.y;
        }
        if (hit_x || hit_y) {
            blocks_[y_block] &= ~(1U << (x_block));
            ApplyBlocks();
            return true;
        }
        return false;
    }
    void Reset() {
        blocks_.fill((1U << num_block.cx) - 1U); // blocks, all visible
        ApplyBlocks();
    }
    WindowBlocks(
        Window* window_parent
    )
        :Window(
            L"Blocks",
            WHITE_BRUSH,
            WS_CHILD,
            SIZE{ 0,0 },
            window_parent
        ) {
        CreateRectRgn(0, 0, 0, 0);
        Reset();
    }
};

class WindowNumber : public Window {
	int number_ = 0;
    void ApplyNumber() {
		auto number = number_;
        number %= 10;
        static uint16_t s_arr_pattern[] = {
            0b111111000111111,
            0b000001111100000,
            0b101111010111101,
            0b111111010110101,
            0b111110010000111,
            0b111011010110111,
            0b111011010111111,
            0b111110000100001,
            0b111111010111111,
            0b111111010110111,
        };
		auto rgn = CreateRectRgn(0, 0, 0, 0);
        auto pattern = s_arr_pattern[number];
        for (int x = 0; x < 3; x++) {
            for(int y=0; y<5; y++) {
                if (pattern & 1) {
                    auto rgn_add = RgnDot(x, y);
					rgn = RgnOr(rgn, rgn_add);
                }
				pattern >>= 1;
 			}
        }
		SetWindowRgn(Handle(), rgn, TRUE);
		DeleteObject(rgn);
	}
public:
    void SetNumber(int number) {
        if (number_ != number) {
            number_ = number;
            ApplyNumber();
        }
    }
    int GetNumber() const { return number_; }
    WindowNumber(
        Window* window_parent
    )
        :Window(
            L"Number",
            WHITE_BRUSH,
            WS_CHILD,
            SIZE{ 0,0 },
            window_parent
        ) {
		CreateRectRgn(0, 0, 0, 0);
        ApplyNumber();
    }
};
class Paddle {
    int width_;
    int width_frame_;
    int x_;
    int rate_ = 1;
public:
    void Move(int delta) {
        x_ += delta;
        if (x_ < 0) x_ = 0;
        int right = (width_frame_ - width_) * rate_;
        if (right < x_) {
            x_ = right;
        }
    }
    int X() const {
        return x_ / rate_;
    }
    Paddle(const Paddle&) = delete;
    Paddle(
        int width,
        int width_frame
    ):  width_(width),
        width_frame_(width_frame),
        x_((width_frame_ - width_) * rate_ / 2) {}
};
Paddle paddle_{ size_paddle.cx, size_frame.cx*size_pixel.cx };
int y_paddle_ = 68;

class MouseOperate {
    int delta_x_ = 0;
public:
    void WmInput(HRAWINPUT hRawInput) {
        UINT dwSize;
        GetRawInputData(hRawInput, RID_INPUT, NULL, &dwSize, sizeof(RAWINPUTHEADER));

        BYTE* lpb = new BYTE[dwSize];
        if (lpb) {
            if (GetRawInputData(hRawInput, RID_INPUT, lpb, &dwSize, sizeof(RAWINPUTHEADER)) == dwSize) {
                RAWINPUT* raw = (RAWINPUT*)lpb;
                if (raw->header.dwType == RIM_TYPEMOUSE) {
                    delta_x_ += raw->data.mouse.lLastX;
                }
            }
            delete[] lpb;
        }
    }
    int DeltaXPop() { auto delta_x = delta_x_; delta_x_ = 0; return delta_x; }
    MouseOperate(HWND hwnd) {
        RAWINPUTDEVICE rid;
        rid.usUsagePage = 0x01;
        rid.usUsage = 0x02;
        rid.dwFlags = RIDEV_INPUTSINK;
        rid.hwndTarget = hwnd;
        RegisterRawInputDevices(&rid, 1, sizeof(rid));
    }
};


struct Coroutine {
    struct promise_type {
        Coroutine get_return_object() { return Coroutine{ std::coroutine_handle<promise_type>::from_promise(*this) }; }
        std::suspend_never initial_suspend() { return {}; }
        std::suspend_never final_suspend() noexcept { return {}; }
        void return_void() {}
        void unhandled_exception() { std::terminate(); }
    };
    std::coroutine_handle<promise_type> handle_;
    Coroutine& operator=(Coroutine&& other) noexcept {
        if (this != &other) {
            if (handle_) handle_.destroy();
            handle_ = other.handle_;
            other.handle_ = nullptr;
        }
        return *this;
    }
    Coroutine(Coroutine&& other) noexcept : handle_(other.handle_) {
        other.handle_ = nullptr;
    }
    Coroutine(std::coroutine_handle<promise_type> h) : handle_(h) {
    }
    ~Coroutine() { if (handle_) handle_.destroy(); }
    bool resume() {
        if (!handle_) {
            return false;
        }
        if (handle_.done()) {
            return false;
        }
        handle_.resume();
        if (handle_.done()) {
            return false;
        }
        return true;
    }
};


LRESULT CALLBACK WindowProcFrame(
    HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam
) {
    static POINT ptPrev = { 0, 0 };
    static BOOL dragging = FALSE;

    switch (uMsg) {
    case WM_KEYDOWN:
        if (wParam == VK_ESCAPE) {
            auto window = Window::FromHwnd(hwnd);
            if (window) {
                window->ShouldClose(true);
            }
            return 0;
        }
        break;
    case WM_LBUTTONDOWN:
        dragging = TRUE;
        ptPrev.x = LOWORD(lParam);
        ptPrev.y = HIWORD(lParam);
        SetCapture(hwnd);
        return 0;

    case WM_MOUSEMOVE:
        if (dragging) {
            POINT ptCurr = { LOWORD(lParam), HIWORD(lParam) };
            RECT rect;
            GetWindowRect(hwnd, &rect);
            MoveWindow(hwnd, rect.left + ptCurr.x - ptPrev.x, rect.top + ptCurr.y - ptPrev.y,
                rect.right - rect.left, rect.bottom - rect.top, TRUE);
        }
        return 0;

    case WM_LBUTTONUP:
        dragging = FALSE;
        ReleaseCapture();
        return 0;

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }

    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

class WindowMouseOperate : public Window {
    std::unique_ptr<MouseOperate> mouse_operate_;
    static LRESULT WndProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam) {
        auto window = Window::FromHwnd(hwnd);
        auto window_mouse_operate =
            reinterpret_cast<WindowMouseOperate*>(
                window
                );
        auto mouse_operate = window_mouse_operate ? window_mouse_operate->mouse_operate_.get() : nullptr;
        switch (uMsg) {
        case WM_INPUT:
            if (mouse_operate) mouse_operate->WmInput((HRAWINPUT)lParam);
            return 0;
        case WM_DESTROY:
            PostQuitMessage(0);
            break;
        }
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    };
public:
    MouseOperate* GetMouseOperate() const {
        return mouse_operate_.get();
    }
    WindowMouseOperate() :
        Window(
            L"MouseOperate",
            BLACK_BRUSH,
            WS_POPUP,
            SIZE{ 0,0 },
            NULL
        ),
        mouse_operate_(new MouseOperate(Handle())){
		SetWindowProc(WndProc);
    }
};
template<auto coroutine>
class WindowWithCoroutine : public Window {
    using CoroutinePtr = std::unique_ptr<Coroutine, void(*)(Coroutine*)>;
    mutable CoroutinePtr coroutine_;
    static LRESULT CALLBACK WindowProc(
        HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam
    ) {
        auto window = Window::FromHwnd(hwnd);
        auto window_with_coroutine =
            reinterpret_cast<WindowWithCoroutine*>(
                window
                );
        switch (uMsg) {
        case WM_TIMER:;
            if (window_with_coroutine == nullptr) {
                return 0;
            }
            if (window_with_coroutine->Step()) {
                return 0;
            }
            KillTimer(hwnd, TIMER_FRAME);
            return 0;

        case WM_DESTROY:
            PostQuitMessage(0);
            break;
        }
        return DefWindowProc(hwnd, uMsg, wParam, lParam);
    }
public:
    bool Step() const {
        if (coroutine_) {
            if (coroutine_->resume()) {
                return true;
            }
            coroutine_.reset();
        }
        return false;
    }
    WindowWithCoroutine(const WindowWithCoroutine&) = delete;
    WindowWithCoroutine(
        LPCWSTR className,
        Window* window_parent
    ) : Window(
        className,
        BLACK_BRUSH,
        WS_EX_APPWINDOW,
        SIZE{ 0,0 },
        window_parent
    ),
        coroutine_(new Coroutine(coroutine(this)), [](Coroutine* c) { delete c; })
    {
        Show(SW_HIDE);
        SetTimer(TIMER_FRAME, static_cast<UINT>(1000 / 59.94 + 0.5), NULL);
        SetWindowProc(WindowProc);
    }
};

static void ApplyScore(
    std::array<WindowNumber, 3>& window_score,
    int score
) {
    for (auto column = 0; column < window_score.size(); column++) {
        window_score[column].SetNumber(score % 10);
        score /= 10;
	}
}

static Coroutine GameMain(Window* window) {
	BeepControl beep_control;
    WindowMouseOperate window_mouse_operate;
    auto mouse_operate = window_mouse_operate.GetMouseOperate();
    Window window_frame{
        L"Frame",
        BLACK_BRUSH,
        WS_POPUP,
        size_frame,
        window
    };
    window_frame.TopMost();
    window_frame.SetWindowProc(WindowProcFrame);
    {
        int screenWidth = GetSystemMetrics(SM_CXSCREEN);
        int screenHeight = GetSystemMetrics(SM_CYSCREEN);
        window_frame.SetPos(
            nullptr,
            screenWidth / 2 - 250, screenHeight / 2 - 250,
            size_frame.cx * size_pixel.cx,
            size_frame.cy * size_pixel.cy,
            SWP_NOZORDER | SWP_SHOWWINDOW
        );
    }
    WindowNumber window_cnt_ball{ &window_frame };
    window_cnt_ball.MoveWindowBlox(1,1,3,5,TRUE);
    std::array<WindowNumber,3> window_score{ &window_frame,&window_frame,&window_frame };
    for (auto column = 0; column < window_score.size(); column++) {
        window_score[column].MoveWindowBlox(
			size_frame.cx - 1 - 3 - 4 * column, 1,
            3, 5, TRUE
        );
    }
	WindowBlocks window_blocks(&window_frame);
    window_blocks.MoveWindowBlox(
        pos_blocks.x, pos_blocks.y,
        16 * size_block.cx,
        num_block.cy * size_block.cy,
        TRUE
	);
    Window window_ball{
        L"Ball",
        WHITE_BRUSH,
        WS_CHILD,
        SIZE{ 1,1 },
        &window_frame
    };
    auto window_paddle = new Window(
        L"Paddle",
        WHITE_BRUSH,
        WS_CHILD,
        size_paddle,
        &window_frame
    );
    window_frame.InvalidateRect(NULL, TRUE);
    window_frame.Update();
    while(true) {
        window_ball.MoveWindow(
            pos_ball.x, pos_ball.y,
            size_pixel.cx, size_pixel.cy, TRUE
        );
        window_paddle->MoveWindow(
            paddle_.X(), y_paddle_ * size_pixel.cy,
            size_paddle.cx, size_paddle.cy * size_pixel.cy,
            TRUE
        );
        window_cnt_ball.SetNumber(0);
        while((GetAsyncKeyState(VK_LBUTTON) & 0x8000)==0) {
            if (window_frame.ShouldClose()
            || (GetAsyncKeyState(VK_RBUTTON) & 0x8000) != 0) {
                co_return;
            }
            co_await std::suspend_always{};
		}
        int score = 0;
        ApplyScore(window_score, score);
        window_blocks.Reset();
        for (int cnt_ball = 3; 0 < cnt_ball; cnt_ball--) {
            window_cnt_ball.SetNumber(cnt_ball);
            pos_ball.x = 10*size_pixel.cx;
			pos_ball.y = 23*size_pixel.cy;
            velocity = { size_pixel.cx, size_pixel.cy };
            do {
                if(window_frame.ShouldClose()
                || (GetAsyncKeyState(VK_RBUTTON) & 0x8000) != 0) {
                    co_return;
                }
                paddle_.Move(mouse_operate->DeltaXPop());
                // 画面端チェック
                bool bound = false;
                if (pos_ball.x + velocity.x < 0 || size_frame.cx*size_pixel.cx <= pos_ball.x + velocity.x + size_pixel.cx) {
                    velocity.x = -velocity.x;
                    bound = true;
                }
                if (pos_ball.y + velocity.y < 0/* || size_frame.cy <= pos_ball.y + velocity.y*/) {
                    velocity.y = -velocity.y;
                    bound = true;
                }
                pos_ball.x -= pos_blocks.x*size_pixel.cx;
                pos_ball.y -= pos_blocks.y*size_pixel.cy;
                if (window_blocks.Collision(pos_ball, velocity)) {
                    score++;
					ApplyScore(window_score, score);
                    bound = true;
                    if (window_blocks.GetRemain() == 0) {
                        window_blocks.Reset();
                    }
                }
                pos_ball.x += pos_blocks.x*size_pixel.cx;
                pos_ball.y += pos_blocks.y*size_pixel.cy;
                if (0 < velocity.y
                    && y_paddle_ * size_pixel.cy<=pos_ball.y + velocity.y
                    && pos_ball.y + velocity.y < y_paddle_ * size_pixel.cy + size_paddle.cy) {
                    // ボールがパドルに当たる
                    if (paddle_.X() <= pos_ball.x && pos_ball.x < paddle_.X() + size_paddle.cx) {
                        velocity.y = -velocity.y;
                        auto vx = velocity.x + pos_ball.x - paddle_.X() - size_paddle.cx / 2;
						vx += (vx < 0) ? -4 : 4; // 左右にずらす
						vx /= 4; // 速度を調整
						velocity.x = vx;
                        bound = true;
                    }
                }
                if(bound) {
                    beep_control.Play();
                }
                pos_ball.x += velocity.x;
                pos_ball.y += velocity.y;
                // 移動
                window_ball.MoveWindow(
                    pos_ball.x, pos_ball.y,
                    size_pixel.cx, size_pixel.cy, TRUE
                );
                window_paddle->MoveWindow(
                    paddle_.X(), y_paddle_* size_pixel.cy,
                    size_paddle.cx, size_paddle.cy* size_pixel.cy,
                    TRUE
                );
                co_await std::suspend_always{};
            } while (pos_ball.y < size_frame.cy*size_pixel.cy);
        }
    }
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
    g_hInstance.reset(new HInstance(hInstance));
    auto window_main =
        new WindowWithCoroutine<GameMain>(
            L"Breakout",
            nullptr
        );
    Window::MessageLoop();
    return 0;
}
